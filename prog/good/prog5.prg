

fun (int, bool)[] areEven(int readonly n)
{
    (int, bool)[] res = new (int, bool)[n];
    for (i = 0 to n - 1)
    {
        res[i] = ~(i, i % 2 == 0);
    }

    return res;
}


fun void main()
{
    int readonly n = 100;

    (int, bool)[] even = areEven(n);
    for (i = 0 to n - 1)
    {
        ~(int readonly liczba, bool readonly czyEven) = even[i];
        printI(liczba);
        printS(": ");
        printLnB(czyEven);
    }
}
