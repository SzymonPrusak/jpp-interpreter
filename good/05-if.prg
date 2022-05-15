

fun void main()
{
    if (true)
        printLnS("Entered here after true condition");

    if (1 == 2)
        printLnS("Entered here after false condition");

    if (2 * 2 + 2 == 8)
    {
        printLnS("This interpreter is not really good at maths");
    }
    else
        printLnS("That was correct calculation");

    if (2 * 2 + 2 == 6)
    {
        printLnS("That was correct calculation");
    }
    else
        printLnS("This interpreter is not really good at maths");
}
