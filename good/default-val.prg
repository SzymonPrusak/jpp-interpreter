
fun string defString() {}
fun bool defBool() {}
fun (string, bool) defTuple() {}

fun (int, bool[], (string, bool)) moreComplexValue() {}

fun void main()
{
    printLnB(defString() == "");
    printLnB(defBool() == false);
    printLnB(defTuple() == ~("", false));
    printLnB(moreComplexValue() == ~(0, new bool[0], ~("", false)));
    printLnB(moreComplexValue() == ~(0, new bool[1], ~("", false)));
}
