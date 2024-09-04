// Print a table of a given function f, computed by taylor series

// function to compute
let f x= (1.0 + x ** 2.0) / 2.0 * atan(x) - (x / 2.0)

let a = 0.1
let b = 0.6
let n = 10
let eps = 0.000001

// Define a function to compute f using naive taylor series method
let taylor_naive x = 
    let rec naive n acc term =
        if abs term < eps then acc, n
        else
            let term = (-1.0) ** (n + 1.0) * (x ** (n * 2.0 + 1.0)) / (4.0 * (n ** 2.0) - 1.0) 
            naive (n + 1.0) (acc + term) term
    naive 1 (0) (x ** 3 / 3.0)


// Define a function to do the same in a more efficient way
let taylor x = 
    let rec taylor_0 n acc term = 
        if abs term < eps then acc, n
        else
          let term = term * (-1.0) * (x**2.0) * (4.0 * n**2.0 - 1.0) / (4.0 * (n + 1.0) ** 2.0 - 1.0)
          taylor_0 (n + 1.0) (acc + term) term
    taylor_0 1 (x ** 3.0 / 3.0) (x ** 3.0 / 3.0)

let main =
   for i=0 to n do
    let x = a+(float i)/(float n)*(b-a)
    let naive, terms1 = taylor_naive x
    let taylor, terms2 = taylor x
    //printfn "| %5.2f  | %10.6f | %10.6f | %.0f | %10.6f | %.0f" x (f x) naive terms1 taylor terms2
    printfn "%5.2f   %10.6f  %10.6f  %.0f  %10.6f  %.0f" x (f x) naive terms1 taylor terms2
// make sure to improve this table to include the required number of iterations
// for each of the methods

main
