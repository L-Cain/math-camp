#' ---
#' title: "5. compiling"
#' author: "Molly Offer-Westort"
#' output: pdf_document
#' ---

#Purpose: compile a nice R script using Molly's nice example
#Clear environment  
rm(list = ls())

## compiling reports ----
# We will compile reports for homework assignments using a method called 
# "spinning." This shows your R code evaluated from start to finish, and shows
# that your results are reproducible. 

#' ## Spinning
#' When spinning, we can use a different kind of comment, roxygen comments, to 
#' do more advanced formatting. I'm switching to roxygen comments now. The
#' formatting only shows up when we compile the document. 
#' 
#' We can make text **bold** or *italicized*. There's lots more formatting one can do with markdown, see here: https://www.markdownguide.org/basic-syntax/
#' 
#' ### You can also create headers and sub-headers for sections. 
#' 
#' And, we can write code inline and have it evaluated using backticks. 
#' For example: `r a + a`. 
# This doesn't work in standard comments: `r a + a`
#' 
#' You can also write equations, just like in Overleaf (because they both use Latex)
#' For example, $Y = \alpha + \beta_0 + \epsilon$. 
#' 
#' 
#' You can also write multi-line equations:
#' 
#' \[
#' \begin{aligned}
#' \int_0^1 x^2\,dx &= \left.\frac{1}{3} x^3\right|_0^1 \\
#' &= \frac{1}{3}1^3 - \frac{1}{3}0^3 \\
#' &= \frac{1}{3}
#' \end{aligned}
#' \]
#' \[
#' \begin{aligned}
#' X & = \frac{15 +30}{2} - \frac{15 + 20 + 20 + 10 + 15}{5}\\
#' & = 6.5
#' \end{aligned}
#' \]
#' 
#' A convenient and helpful "cheatsheet" for LaTeX math is here: 
#' http://reu.dimacs.rutgers.edu/Symbols.pdf
#' 
#' A tool on the web that might be helpful for writing expressions in a 
#' relatively intuitive way is here:
#' http://www.codecogs.com/latex/eqneditor.php?lang=en-en, 
#' where you click on symbols and things, and the latex code comes out in the 
#' yellow box.
#' 
#' You can also include images, using the `knitr::include_graphics` function. 

#+ out.width="50%"
knitr::include_graphics("../data/Zenos.jpg")

#' We set the scale of the picture using the out.width argument (here, it's 50%)
#' of the page. 
#' 
#' And we can include plots generated in R. 
plot(dnorm, -3, 3, main = "A bell curve", ylab = "Density", xlab = "Value", 
     type="l")
abline(v = 0, lwd = 2, col = "blue") # a vertical line
abline(h = dnorm(0), lty=2, col = "red") # a horizontal dotted line


#'  _A note on compiling:_ Reports are compiled using a knitr function called
#'  "spin." You can look up `knitr::spin` if you want to learn more options for 
#'  your text and code presentation. Knitr uses Markdown formatting, which you 
#'  can look up for things like styling text and creating lists. 
#'  
#'  Let's compile the document now.
#'  
#'  Exercise 1: We hit an error, so let's find and correct it. 
#'  Now that it's working, what if we want to change where the compiled output lives?

