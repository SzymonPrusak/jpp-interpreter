
fun int fib(int n)
{
    if (n <= 2)
        return 1;
    return fib(n - 1) + fib(n - 2);
}

fun void main()
{
    for (i = 1 to 100)
        printLnI(fib(i));
}
