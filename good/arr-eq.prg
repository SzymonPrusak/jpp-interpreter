

fun void main()
{
    int[] readonly a = [1,2,3];
    int[] readonly b = a;
    int[] readonly c = [4,5,6,7,8];

    printLnB(a == b);
    printLnB(b == c);

    string[][] readonly d = [["aaa"], ["bbbb", "cccc"]];
    string[][] readonly e = d;
    string[][] readonly f = [["aaa"], ["bbbbc", "cccc"]];

    printLnB(d == e);
    printLnB(d == f);
}
