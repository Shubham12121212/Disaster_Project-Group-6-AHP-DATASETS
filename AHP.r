#' Calculate analytic hierarchy process weights
#'
#' The analytic hierarchy process (AHP) can be used for multi-criteria
#' decision analysis.  It performs a pairwise comparison of a list of
#' options, with a respect to a particular goal.
#'
#' @detail The script will ask you to compare pairs of options.
#'  Answer each question with respect to the statement goal and use
#'  scores from 1,3,5,7,9 (or their inverse) where 1 = equally
#'  important, 3 = moderately preferred, 5 = strongly preferred, 7 =
#'  very strongly preferred, 9 = extremely preferred.  For example, if
#'  A is moderately preferred to B in respect of the goal, answer:
#'  Compare A to B: 3, or Compare B to A: 1/3
#'
#' @author James Keirstead \email{j.keirstead@@imperial.ac.uk}
#' @param options a character vector of options
#' @param goal a character object describing the goal
#' @return a list containing the weights of each option, the
#' consistency ratio, and intermediate calculation values.  The
#' weights should only be used if the consistency ratio is less than
#' or equal to 0.1.  Otherwise, the analysis should be repeated and
#' new answers given.
#' @source TODO: I know this is based on some code I once found but
#' not sure where.  Wikipedia
#' \link{http://en.wikipedia.org/wiki/Analytic_hierarchy_process} has
#' some good examples of this might be applied.
#' @examples
#' \dontrun{
#' fruits <- c("apple", "orange", "banana")
#' goal <- "taste"
#' AHP(fruits, goal)
#' }
AHP <- function(options, goal) {

  ## Create a matrix to store the results
  n <- length(options)
  mat <- diag(n)

  ## Randomly mix the categories and get user to populate matrix
  comb <- combn(options, 2)
  o <- sample(1:ncol(comb))
  for (i in o) {
    c <- which(options==comb[1,i])
    r <- which(options==comb[2,i])
    val <- readline(sprintf("Compare %s to %s with respect to %s (1--9): ",
                            options[r], options[c], goal))
    mat[c, r] <- eval(parse(text=val))
  }    

  mat <- t(mat) + mat
  mat[upper.tri(mat)] <- 1/mat[upper.tri(mat)]
  diag(mat) <- 1

  ## Calculate the results
  result <- eigen (mat)
  val <- as.numeric(result$values[1])
  vec <- as.numeric(result$vectors[,1])
  weight <- vec /sum(vec)
  ci <- (val-n)/(n-1)
  cr <- ci/c(0,0,0.58,0.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.53)[n]
  if (ci > 0.1 || cr > 0.1) {
    cat("\n CI = ",ci,", CR = ", cr, "\n", sep="")
    print(mat)
    W <- outer(weight, weight,"/")
    print(W)
    print(mat-W)
  }

  ## Return a summary list
  return (list (mat=mat, lambda=val,vec=vec, weight=weight,ci=ci,cr=cr))
}
