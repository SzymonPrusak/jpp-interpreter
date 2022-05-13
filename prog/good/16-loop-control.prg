

fun void main()
{
    int i = 0;
    while (true)
    {
        if (i == 10)
            break;
        i = i + 1;
        if (i % 2 == 0)
            continue;
        printLnS("work 1");
    }

    for (i2 = 0 to 10000000)
    {
        if (i2 == 10)
            break;
        i = i + 1;
        if (i % 2 == 0)
            continue;
        printLnS("work 2");
    }
}
