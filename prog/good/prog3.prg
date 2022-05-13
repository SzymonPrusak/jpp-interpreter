fun void main(string b)
{
    int a;
    a = 5;

    printLnI(a);
    printLnS(b);
    printLnS("aaaa");
    printLnI(5);
    printLnB(false);

    bool t;
    t = 5 > 4;
    
    while (t && 2 != 2)
        printLnS("it's true");

    int i;
    i = 5;
    while (i < 10)
    {
        printLnI(i);
        i = i + 1;
    }
}
