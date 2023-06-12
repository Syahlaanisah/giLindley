# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Hasil:
KS.testGIL <- function(x){
  # Fungsi Kepadatan Kumulatif
  pGenilindley <- function(x, alpha,lambda)
  {
    a = (1+lambda+(lambda*x^(-alpha)))/(1+lambda)
    b = exp((-lambda)*x^(-alpha))
    result = a*b
    return(result)
  }
  pGenilindley (x, alpha = 0.5177663,lambda = 1803.86)

  # Fungsi Kepadatan Peluang
  dGenilindley <- function(x, alpha,lambda)
  {
    f=((alpha*lambda^2)/(lambda+1))*((1+x^(-alpha))*x^(-alpha-1)*exp(-lambda*x^(-alpha)))
    result = f
    return(result)
  }
  dGenilindley (x, alpha = 0.5177663,lambda = 1803.86)

  m=sort(unique(unlist(x)),decreasing = FALSE)
  #FS adalah fungsi distribusi kumulatif dari distribusi dengan menggunakan data unik
  FS = pGenilindley (m, alpha = 0.5177663,lambda = 1803.86)
  frek = as.matrix(table(x))
  FT = cumsum(frek)/length(x)
  D =max(abs(FT-FS))
  D
}
KS.testGIL(x)
