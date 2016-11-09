snakeize <- function(camel_str) {
  s <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", camel_str, perl = TRUE)
  sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case
}

parse_kv <- function(str, sep='_') {
  do.call(sprintf, as.list(unlist(c('%s="%s"', str_split(str, sep)))))
}

wrap_quote <- function(el, need_quote) {
  if (is.character(el) && need_quote) {
    sprintf('"%s"', el)
  } else if (is.language(el)) {
    as.character(list(el))
  } else as.character(el)
}

clist <- function(arg_lst, need_quote=T) {
  arg_lst <- arg_lst[!sapply(arg_lst, is.null)]
  paste(
    sapply(1:length(arg_lst), function(par_i) 
    sprintf('%s=%s', names(arg_lst)[par_i], wrap_quote(arg_lst[[par_i]], need_quote))),
    collapse=', ')
}

generateCode <- function(p) {
  state <- sys.frames()[[1]]
  
  if ('ggmatrix' %in% class(p)) {
    cols <- paste(sapply(p$xAxisLabels, function(w) sprintf('"%s"', w)), collapse=', ')
    mapping <- p$plots[[1]]$mapping
    print_mapping <- mapping[setdiff(names(mapping), c('x', 'y'))]
    map_aes <- if (length(print_mapping)) 
      sprintf(' mapping=aes(%s),', clist(print_mapping)) else ''
    adjustment <- if (is.null(state$pairs)) '' else paste0(', ',
      clist(lapply(state$pairs, function(x) sprintf('list(%s)', clist(x))), F))
    
    return(sprintf('ggpairs(%s,%s columns=c(%s)%s)',
                   state$dataset_name, map_aes, cols, adjustment))
  }
  
  p$mapping <- rev(p$mapping)
  res_dataset_name <- state$dataset_name
  
  if (!is.null(state$lim_range)) {
    lim_range <- state$lim_range
    res_dataset_name <- sprintf('data.table(%s)', res_dataset_name)
    for (i in 1:length(lim_range)) {
      ax <- lim_range[[i]]
      ax_name <- names(lim_range)[[i]]
      isNum <- is.numeric(ax$val)
      
      res_dataset_name <- sprintf('%s[%s %s c(%s)]',
        res_dataset_name, deparse(p$mapping[[ax_name]]), 
        if (isNum) '%between%' else '%in%',
        paste(if (isNum) round(ax$val, 2) else 
          gsub('\\b', '\\"', ax$val, perl = T), collapse=', '))
    }
  }
  res <- sprintf('ggplot(%s, aes(%s))', res_dataset_name, clist(p$mapping))
  
  for (layer in p$layers) {
    if (any(class(layer$geom) == 'Geom')) {
      geom_name <- snakeize(class(layer$geom)[[1]])
      geom_params <- paste(parse_kv(snakeize(class(layer$stat)[[1]])), 
                           parse_kv(snakeize(class(layer$position)[[1]])), sep=', ')
      if (!is.null(layer$aes_params) && length(layer$aes_params)) {
        geom_params <- paste(geom_params, clist(layer$aes_params), sep=', ')
      }
      if (!is.null(layer$mapping) && length(layer$mapping)) {
        aes_mapping_mask <- sapply(layer$mapping, function(x) !is.null(x))
        if (any(aes_mapping_mask)) {
          geom_params <- sprintf('aes(%s), %s', clist(layer$mapping[aes_mapping_mask]), 
                                 geom_params)
        }
      }
      # for (params in c(layer$stat_params, layer$geom_params)) {
      # collision na.rm=F for both stat_params and geom_params
      param_mask <- sapply(layer$stat_params, function(x)
        !is.null(x) && (!is.logical(x) || x) && (!is.numeric(x) || x > 0) &&
          ((!is.vector(x) && !is.list(x)) || length(x) > 0))
      if (any(param_mask)) {
        useful_params <- layer$stat_params[param_mask]
        geom_params <- paste(geom_params, clist(useful_params), sep=', ')
        if ('bins' %in% names(useful_params) && geom_name == 'geom_bar') {
          geom_name <- 'geom_histogram'
        }
      }
      res <- sprintf('%s + %s(%s)', res, geom_name, geom_params)
    }
  }
  
  if ('guides' %in% names(p)) {
    guide_params <- c()
    for (guide_i in 1:length(p$guides)) {
      guide_name <- names(p$guides)[guide_i]
      guide <- p$guides[[guide_i]]
      if (!is.null(guide$title) && guide_name != guide$title) {
        guide_params <- c(guide_params, sprintf('%s=guide_legend(title="%s")', 
                                                guide_name, guide$title))
      }
    }
    if (length(guide_params)) {
      res <- sprintf('%s + guides(%s)', res, paste(guide_params, collapse=', '))
    }
  }
  
  for (scale in p$scales$scales) {
    if (scale$aesthetics == 'size') {
      # I have found that range information stores as an inner function
      # print(scale_size) -> continuous_scale(.., palette=area_pal(range)) -> 
      # ... -> ggproto(palette=palette)
      # so we can restore range's low and high by palette() out solving equation: 
      # sqrt(in)*(high-low)+low=out
      low <- p$scales$scales[[1]]$palette(0)
      high <- p$scales$scales[[1]]$palette(1)
      res <- sprintf('%s + scale_size(range=c(%s, %s))', res, low, high)
    } else if (scale$aesthetics == 'colour') {
      res <- sprintf('%s + scale_color_%s()', res, scale$scale_name)
    }
  }
  
  if (class(p$coordinates)[1] == 'CoordFlip') {
    res <- sprintf('%s + %s()', res, snakeize(class(p$coordinates)[1]))
  }
  
  if (!is.null(state$theme_name)) {
    res <- sprintf('%s + theme_%s()', res, state$theme_name)
  }
  if (!is.null(state$theme_attrs)) {
    res <- sprintf('%s + theme(text=element_text(%s))', res, clist(state$theme_attrs))
  }
  
  for (lab in c('title', 'x', 'y')) {
    if (!is.null(p$labels[[lab]])) {
      res <- sprintf('%s + %s("%s")', res, 
                     if (lab == 'title') 'ggtitle' else paste0(lab, 'lab'), 
                     p$labels[[lab]])
    }
  }
  
  if (all(class(p$facet) != 'null')) {
    res <- sprintf('%s + %s', res, format(p$facet) %>% 
                     gsub(',', ' +', .) %>% 
                     gsub('\\( ~', '(. ~', .) %>% 
                     gsub('~ \\)', '~ .)', .) %>%
                     gsub('\\(', '("', .) %>% 
                     gsub('\\)', '")', .))
    free_mask <- unlist(p$facet$free)
    if (any(free_mask)) {
      res <- sub(')$', sprintf(
        ', scales="free%s")', 
        if (all(free_mask)) '' else if (free_mask['x']) '_x' else '_y'), res)
    }
  }
  
  res
}

