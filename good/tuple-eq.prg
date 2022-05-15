

fun void main()
{
    (int, bool) a = ~(5, true);
    (int, bool) b = a;
    (int, bool) c = ~(6, true);

    printLnB(a == b);
    printLnB(b == c);
}
