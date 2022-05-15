

fun (int, (bool, bool))[] areEven(int readonly n)
{
    (int, (bool, bool))[] res = new (int, (bool, bool))[n];
    for (i = 0 to n - 1)
    {
        res[i] = ~(i, ~(i % 2 == 0, i % 2 == 1));
    }

    return res;
}


fun void printTuple1((int, (bool, bool)) readonly t)
{
    ~(int readonly liczba, (bool, bool) readonly t2) = t;
    ~(bool readonly czyEven, bool readonly czyOdd) = t2;
    printI(liczba);
    printS(": ");
    printB(czyEven);
    printS(", ");
    printLnB(czyOdd);
}


fun void printTuple2((int, (bool, bool)) readonly t)
{
    ~(int readonly liczba, ~(bool readonly czyEven, bool readonly czyOdd)) = t;

    printI(liczba);
    printS(": ");
    printB(czyEven);
    printS(", ");
    printLnB(czyOdd);
}


fun void main()
{
    (int, (bool, string, int)[][], string, (int, string)) complexTuple;
    
    int readonly n = 100;

    (int, (bool, bool))[] even = areEven(n);
    for (i = 0 to n - 1)
    {
        if (i % 2 == 0)
        {
            printTuple1(even[i]);
        }
        else
            printTuple2(even[i]);
    }
}
