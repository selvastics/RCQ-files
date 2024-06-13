figcapabove_hook_plot_tex = function(x, options) {
  `%n%` <- knitr:::`%n%`
  library(xfun)

  rw = options$resize.width
  rh = options$resize.height
  rc = options$resize.command
  resize1 = resize2 = ''
  if (is.null(rc)) {
    if (!is.null(rw) || !is.null(rh)) {
      resize1 = sprintf('\\resizebox{%s}{%s}{', rw %n% '!', rh %n% '!')
      resize2 = '} '
    }
  } else {
    # users can specify a custom "resize" command (we can use an arbitrary
    # command, e.g., framebox)
    resize1 = paste0('\\', rc, '{')
    resize2 = '} '
  }

  tikz = knitr:::is_tikz_dev(options)

  a = options$fig.align
  fig.cur = options$fig.cur %n% 1L
  fig.num = options$fig.num %n% 1L
  animate = options$fig.show == 'animate'
  fig.ncol = options$fig.ncol %n% fig.num
  if (is.null(fig.sep <- options$fig.sep)) {
    fig.sep = character(fig.num)
    if (fig.ncol < fig.num) fig.sep[seq(fig.ncol, fig.num - 1L, fig.ncol)] = '\\newline'
  }
  sep.cur = NULL

  # If this is a non-tikz animation, skip to the last fig.
  if (!tikz && animate && fig.cur < fig.num) return('')

  usesub = length(subcap <- options$fig.subcap) && fig.num > 1
  # multiple plots: begin at 1, end at fig.num
  ai = options$fig.show != 'hold'

  # TRUE if this picture is standalone or first in set
  plot1 = ai || fig.cur <= 1L
  # TRUE if this picture is standalone or last in set
  plot2 = ai || fig.cur == fig.num

  # open align code if this picture is standalone/first in set
  align1 = if (plot1)
    switch(a, left = '\n\n', center = '\n\n{\\centering ', right = '\n\n\\hfill{}', '\n')
  # close align code if this picture is standalone/last in set
  align2 = if (plot2)
    switch(a, left = '\\hfill{}\n\n', center = '\n\n}\n\n', right = '\n\n', '')

  # figure environment: caption, short caption, label
  cap = options$fig.cap
  scap = options$fig.scap
  fig1 = fig2 = ''
  mcap = fig.num > 1L && options$fig.show == 'asis' && !length(subcap)
  # initialize subfloat strings
  sub1 = sub2 = ''

  # Wrap in figure environment only if user specifies a caption
  if (length(cap) && !is.na(cap)) {
    lab = paste0(options$fig.lp, options$label)
    # If pic is standalone/first in set: open figure environment
    if (plot1) {
      pos = options$fig.pos
      if (pos != '' && !grepl('^[[{]', pos)) pos = sprintf('[%s]', pos)
      fig1 = sprintf('\\begin{%s}%s', options$fig.env, pos)
    }
    # Add subfloat code if needed
    if (usesub) {
      sub1 = sprintf('\\subfloat[%s%s]{', subcap, knitr:::create_label(lab, '-', fig.cur, latex = TRUE))
      sub2 = '}'
      sep.cur = fig.sep[fig.cur]
      # when there are more separators than plots, add the first separator
      # before the first plot, then the (i+1)th separator to the i-th plot
      if (length(fig.sep) > fig.num) {
        if (plot1) sub1 = paste0(fig.sep[1], sub1)
        sep.cur = fig.sep[fig.cur + 1]
      }
      if (is.na(sep.cur)) sep.cur = NULL
    }

    # If pic is standalone/last in set:
    # * place caption with label
    # * close figure environment
    if (plot2) {
      if (is.null(scap) && !grepl('[{].*?[:.;].*?[}]', cap)) {
        scap = strsplit(cap, '[:.;]( |\\\\|$)')[[1L]][1L]
      }
      scap = if (is.null(scap) || is.na(scap)) '' else sprintf('[%s]', scap)
      cap = if (cap == '') '' else sprintf(
        '\\caption%s{%s}%s\n', scap, cap,
        knitr:::create_label(lab, if (mcap) c('-', fig.cur), latex = TRUE)
      )
      fig2 = sprintf('\\end{%s}\n', options$fig.env)  # <------------- Removed cap from here
    }
  } else if (knitr:::pandoc_to(c('latex', 'beamer'))) {
    # use alignment environments for R Markdown latex output (\centering won't work)
    align.env = switch(a, left = 'flushleft', center = 'center', right = 'flushright')
    align1 = if (plot1) if (a == 'default') '\n' else sprintf('\n\n\\begin{%s}', align.env)
    align2 = if (plot2) if (a == 'default') '' else sprintf('\\end{%s}\n\n', align.env)
  }

  ow = options$out.width
  # maxwidth does not work with animations
  if (animate && identical(ow, '\\maxwidth')) ow = NULL
  if (is.numeric(ow)) ow = paste0(ow, 'px')
  size = paste(c(sprintf('width=%s', ow),
                 sprintf('height=%s', options$out.height),
                 options$out.extra), collapse = ',')

  paste0(
    fig1, align1, sub1, resize1, cap, # <------------- Moved cap to here
    if (tikz) {
      sprintf('\\input{%s}', x)
    } else if (animate) {
      # \animategraphics{} should be inserted only *once*!
      aniopts = options$aniopts
      aniopts = if (is.na(aniopts)) NULL else gsub(';', ',', aniopts)
      size = paste(c(size, sprintf('%s', aniopts)), collapse = ',')
      if (nzchar(size)) size = sprintf('[%s]', size)
      sprintf('\\animategraphics%s{%s}{%s}{%s}{%s}', size, 1 / options$interval,
              sub(sprintf('%d$', fig.num), '', sans_ext(x)), 1L, fig.num)
    } else {
      if (nzchar(size)) size = sprintf('[%s]', size)
      res = sprintf(
        '\\includegraphics%s{%s} ', size,
        if (getOption('knitr.include_graphics.ext', FALSE)) x else sans_ext(x)
      )
      lnk = options$fig.link
      if (is.null(lnk) || is.na(lnk)) res else sprintf('\\href{%s}{%s}', lnk, res)
    },

    resize2, sub2, sep.cur, align2, fig2
  )
}