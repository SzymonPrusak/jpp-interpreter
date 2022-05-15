

fun void a()
{
    printLnS("Still bound statically!");
}

fun void d()
{
    a();
}


fun void b()
{
    fun void c()
    {
        fun void a()
        {
            printLnS("Not bound statically :(");
        }

        d();
    }

    a();
    c();
}


fun void main()
{
    b();
}
