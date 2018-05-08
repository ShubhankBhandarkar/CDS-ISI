
Below are some of my observations from this assignment problems:
1. For both Linear and Binary Search average steps required were ~2-3. Reason being, random number generated had uniform local peaks.
2. For 2D Peak Search, complexity should be n*log(n) but on an average I got n complexity for the same reason as above. For higher values i.e 1000*1000 matrix, I got 1000 steps for nearly all the iterations as the range of random number is -999 to 999 and thus there is higher probability of getting the highest number in the 1st column of selection.
3. For precision of 15 digits, Binary Square Root took on an average 55-60 steps to converge whereas Newton Square Root converged much faster in ~6-7 steps.
4. In Newton-Raphson approach too, error converged in 6-7 steps to 10^(-16). Convergence Rate i.e. e(i+1)/(e(i)^2) came out to be 0.426 for function x^3 - 13. Below is the plot for error vs steps.
5. In Secant Method, convergence depends on the initial guess. For the guess of 0 and 10 in function x^3 - 13 this method took nearly 67 steps to converge to 10^-10 and for guess 0 and 3 same converged in 9 steps. Whereas convergence rate for the final term was same as in newton-raphson method. Below is the chart for error:

To learn more, I have tried to use precision in some functions where user can input the precision and function returns value upto that precision. I used decimal library for this to get more decimal values than normal numpy. (rounding off the digits after precision limit).
