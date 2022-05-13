

fun int a()
{

}

fun int main()
{
    fun int b()
    {
        fun int a()
        {
            printLnS("not linked statically :(");
        }
        main();
    }

    printLnS("still linked statically!");
    a();
    b();
}