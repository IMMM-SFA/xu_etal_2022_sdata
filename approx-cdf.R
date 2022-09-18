## from github.com/brookslogan/snippets

## MIT License
##
## Copyright (c) 2022 brookslogan
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

library("dplyr")
library("purrr")
library("ggplot2")
library("pipeR")
library("rlang")
library("assertthat")

## piecewise linear + jumps cadlag function starting with y=0 ending with any nonneg y, constant beyond edges
approx_cdf = function(xs, ys.before, ys.at) {
  assert_that(is.numeric(xs))
  assert_that(is_double(ys.before))
  assert_that(is_double(ys.at))
  assert_that(!any(is.na(xs)))
  assert_that(!any(is.na(ys.before)))
  assert_that(!any(is.na(ys.at)))
  assert_that(all(ys.before >= 0))
  assert_that(all(ys.at >= 0))
  assert_that(length(xs) >= 1L)
  assert_that(length(xs) == length(ys.before))
  assert_that(length(ys.before) == length(ys.at))
  assert_that(!is.unsorted(xs))
  assert_that(!any(xs[-length(xs)] == xs[-1L]))
  assert_that(!is.unsorted(ys.before))
  assert_that(all(ys.before <= ys.at))
  assert_that(all(ys.at[-length(ys.before)] <= ys.before[-1L]))
  assert_that(ys.before[[1L]] == 0)
  assert_that(ys.at[[length(ys.at)]] == 1)
  ##
  unclassed = list(xs=xs, ys.before=ys.before, ys.at=ys.at)
  result = `class<-`(unclassed, "approx_cdf")
  return (result)
}

plot.approx_cdf = function(x, y, ...) {
  unclassed = unclass(x)
  xs = unclassed[["xs"]]
  ys.before = unclassed[["ys.before"]]
  ys.at = unclassed[["ys.at"]]
  ggplot(data.frame(x=c(-Inf, rep(xs, each=2L), +Inf),
                    y=c(ys.before[[1L]], as.vector(rbind(ys.before, ys.at)), ys.at[[length(ys.at)]])),
         aes(x, y)) %>>%
    `+`(geom_path()) %>>%
    `+`(geom_point())
}

## TODO simplify_approx_cdf

weighted_mean_approx_cdfs = function(approx.cdfs, weights) {
  assert_that(is_list(approx.cdfs))
  assert_that(all(map_lgl(approx.cdfs, inherits, "approx_cdf")))
  assert_that(is_double(weights))
  assert_that(all(weights >= 0))
  assert_that(abs(sum(weights) - 1) < 1e-8)
  assert_that(length(approx.cdfs) == length(weights))
  assert_that(length(approx.cdfs) >= 1L)
  ##
  result.xs =
    approx.cdfs %>>%
    map("xs") %>>%
    reduce(union) %>>%
    sort()
  result.y.jumps.and.jumpless.ys =
    map2(approx.cdfs, weights, function(approx.cdf, weight) {
      old.unclassed = unclass(approx.cdf)
      old.xs = old.unclassed[["xs"]]
      old.ys.before = old.unclassed[["ys.before"]]
      old.ys.at = old.unclassed[["ys.at"]]
      old.y.jumps = old.ys.at - old.ys.before
      old.jumpless.ys = old.ys.at - cumsum(old.y.jumps)
      new.y.jumps = weight * `[<-`(rep(0, length(result.xs)),
                                   match(old.xs, result.xs),
                                   old.y.jumps)
      ## TODO store a rule in the approx_cdf?
      new.jumpless.ys = weight * approx(old.xs, old.jumpless.ys, result.xs, rule=2L)[["y"]]
      list(new.y.jumps=new.y.jumps, new.jumpless.ys=new.jumpless.ys)
    }) %>>%
    reduce(function(contribs1, contribs2) {
      list(
        new.y.jumps = contribs1[["new.y.jumps"]] + contribs2[["new.y.jumps"]],
        new.jumpless.ys = contribs1[["new.jumpless.ys"]] + contribs2[["new.jumpless.ys"]]
      )
    })
  result.y.jumps = result.y.jumps.and.jumpless.ys[["new.y.jumps"]]
  result.jumponly.ys = cumsum(result.y.jumps)
  result.jumpless.ys = result.y.jumps.and.jumpless.ys[["new.jumpless.ys"]]
  prescale.result.ys.at = result.jumponly.ys + result.jumpless.ys
  ## prescale.result.ys.before = prescale.result.ys.at - result.y.jumps # can produce unsorted results, e.g., due to loss in precision when there is catastrophic cancellation
  prescale.result.ys.before = c(0, result.jumponly.ys[-length(result.jumponly.ys)]) + result.jumpless.ys
  ## scale to make sure ys.at[[length(ys.at)]] is exactly 1:
  result.ys.at = prescale.result.ys.at / prescale.result.ys.at[[length(prescale.result.ys.at)]]
  result.ys.before = prescale.result.ys.before / prescale.result.ys.at[[length(prescale.result.ys.at)]]
  result = approx_cdf(
    xs = result.xs,
    ys.before = result.ys.before,
    ys.at = result.ys.at
  )
  return (result)
}

## TODO an evaluation function? or refactor to make this a classed function? and maybe rewrite the weighted combination to use the evaluation function?

quantile.approx_cdf = function(x, probs, ...) {
  assert_that(is_double(probs))
  assert_that(all(0 <= probs & probs <= 1))
  ##
  unclassed = unclass(x)
  path.xs = rep(unclassed[["xs"]], each=2L)
  path.ys = as.vector(rbind(unclassed[["ys.before"]], unclassed[["ys.at"]]))
  ## find where the probs lie along the path.ys; use findInterval with
  ## left.open=TRUE to choose the earliest interval containing each prob rather
  ## than the latest, and all.inside=TRUE so that a prob of 0 is mapped to index
  ## 1L, not 0L:
  left.path.indices = findInterval(probs, path.ys, left.open=TRUE, all.inside=TRUE)
  is.between.jumps = left.path.indices %% 2L == 0L
  jump.path.indices = left.path.indices[!is.between.jumps]
  between.jump.left.path.indices = left.path.indices[is.between.jumps]
  double(length(left.path.indices)) %>>%
    `[<-`(!is.between.jumps, path.xs[jump.path.indices]) %>>%
    `[<-`(is.between.jumps,
      (  probs[is.between.jumps]                  - path.ys[between.jump.left.path.indices]) /
      (path.ys[between.jump.left.path.indices+1L] - path.ys[between.jump.left.path.indices]) *
      (path.xs[between.jump.left.path.indices+1L] - path.xs[between.jump.left.path.indices]) +
                                                    path.xs[between.jump.left.path.indices])
}

approx_cdf_from_quantiles = function(quantiles, probs) {
  assert_that(is.numeric(quantiles))
  assert_that(!is.unsorted(quantiles))
  assert_that(is_double(probs))
  assert_that(all(0 <= probs & probs <= 1))
  ##
  rle.quantiles = rle(quantiles)
  cdf.xs = rle.quantiles[["values"]]
  cdf.ys.at.inds = cumsum(rle.quantiles[["lengths"]])
  cdf.ys.before.inds = cdf.ys.at.inds - rle.quantiles[["lengths"]] + 1L
  cdf.ys.at = `[<-`(probs[cdf.ys.at.inds], length(cdf.xs), 1)
  cdf.ys.before = `[<-`(probs[cdf.ys.before.inds], 1L, 0)
  approx_cdf(cdf.xs, cdf.ys.before, cdf.ys.at)
}
