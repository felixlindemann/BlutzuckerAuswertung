Lab.palette <-
function (n) 
{
    x <- ramp(seq.int(0, 1, length.out = n))
    rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
}
