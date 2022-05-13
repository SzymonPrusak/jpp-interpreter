
fun int fib(int readonly n)
{
    if (n <= 2)
        return 1;
    
    int[] fibs;
    fibs = new int[n];
    fibs[0] = 1;
    fibs[1] = 1;

    for (i = 2 to n - 1)
        fibs[i] = fibs[i - 1] + fibs[i - 2];

    return fibs[n - 1];
}

fun void main()
{
    int n;
    n = 100;

    for (i = 0 to n)
        printLnI(fib(1));
}
