#' rerun a function call in case of error
#'
#' If a call returns an error, run it again.
#'
#' Iterative calling of a function that can return a random error
#' can crash a process or at the very least result in incomplete data.
#' To alleviate the impact of random errors the failed call will be rerun
#' until it succeeds or until a limit of attempts is reached.
#' If all attempts fail, a backup value will be returned.
#'
# Note that the iteration counter is reset only when \code{retryFO} is called
# and the iteration count will stack across multiple calls of modified \code{foo}.
# Conversely, \code{retry} runs resets the counter before it first evaluates \code{call}.
#'
#' @param call call to retry
#' @param max.iter maximum number of iterations after which failure is declared
#' @param fail object to return in case of failure
#' @param verbose logical flag whether to print iterations as messages
#'
#' @return \code{retry} returns the same as the function in \code{call}
#' or as defined by \code{fail}.
#'
# \code{retryFO} returns a modified version of \code{foo} that reruns itself.
#'
#' @export
#'
#' @examples
#' f <- function() {
#'   x <- sample(1:2, size = 1)
#'   if (x == 1) stop("it's a 1", call. = TRUE)
#'   return(x)
#' }
#' \dontrun{
#' f()
#' replicate(10, f())
#' }
#'
#' retry(f(), 2, "failed", TRUE)
#' table(replicate(100, retry(f(), 2, "failed", FALSE)))
#'
# @describeIn retry
# attepmts to evaluate \code{call} untill it succeeds or a limit is reached
#'
retry <- function(call, max.iter, fail = NA, verbose = FALSE) {
  C <- substitute(call)
  if (!is.call(C)) stop('"call" must be a function call')
  if (!is.numeric(max.iter)) stop('"max.iter" must be numeric')
  if (max.iter > 1000) warning('a large number of attempts has been requested')

  # define constructor for counting runner
  G <- function() {
    iter <- 0
    g <- function() {
      iter <<- iter +1
      if (verbose) message('iteration ', iter)
      tryCatch(eval(C), error = function(e) {if (iter < max.iter) g() else (return(fail))})
    }
    return(g)
  }
  # run constructor to create counting runner
  h <- G()
  # run counting runner
  h()
}

# #' @param foo a function
# #' @describeIn retry
# #' returns modified \code{foo} that retries itself untill it succeeds or a limit reached
# #'
# retryFO <- function(foo, max.iter, fail = NA, verbose = FALSE) {
#   if (!is.function(foo)) stop('"foo" must be a function')
#   if (!is.numeric(max.iter)) stop('"max.iter" must be numeric')
#   if (max.iter > 1000) warning('a large number of attempts has been requested')
#
#   # define constructor for counting runner
#   G <- function() {
#     iter <- 0
#     g <- function() {
#       iter <<- iter +1
#       if (verbose) message('iteration', iter)
#       tryCatch(foo(), error = function(e) {if (iter < max.iter) g() else (return(fail))})
#     }
#     return(g)
#   }
#   # run constructor to create counting runner
#   h <- G()
#   # return counting runner
#   return(h)
# }

# @examples continued
# \dontrun{
# f.fo <- retryFO(f, 2, "failed", TRUE)
# f.fo()
# table(replicate(10, f.fo()))
# }
#




# # What to do about randomly occurring errors?
#
# # function f that randomly generates error on every 1 out of 2 runs
# f <- function() {
#   x <- sample(x = 1:2, size = 1)
#   if (x == 1) stop('it\'s a 1', call. = TRUE)
#   return(x)
# }
# # function g that runs f until there is no error and returns result
# # possibly infinite recursion
# g <- function() {
#   tryCatch(f(), error = function(e) g())
# }
#
# # function that returns modified g that also counts iterations and
#   # apologizes if iteration is higher than set (max)
# G <- function(max) {
#   iter <- 0
#   g <- function() {
#     iter <<- iter +1
#     #cat('iteration', iter, '\n')
#     tryCatch(f(), error = function(e) {if (iter < max) g() else (return('przepraszam'))})
#   }
#   return(g)
# }
# # construct counting rerunner
# h <- G(max = 5)
# # all calls to g are counted henceforth
# h()
# # function that construcs a fresh counting runner and immediately runs it
# # allows setting the maximum number of iterations at this stage
# H <- function(...) {
#   h <- G(...)
#   h()
# }
#
# # test it
# invisible(sapply(1:10, function(x) table(replicate(1000, H(max = x)))))
# it <- sapply(1:10, function(x) table(replicate(1000, H(max = x))))
# dimnames(it)[[2]] <- 1:10
# barplot(it,
#         las = 1,
#         main = 'jak czesto przeprasza',
#         xlab = 'ile razy probuje',
#         legend.text = dimnames(it)[[1]])
