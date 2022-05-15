

fun int main()
{
    int a = 5 * 6 / 10;
    string s1 = "aaaa";
    string s2 = "bbbb";
    string s3 = "aaaa";
    int b = a;

    bool bi1 = a == b;
    bool bi2 = 1 > a;
    bool bs1 = s1 == s3;
    bool bs2 = s2 != s3;
    bool bb1 = true == false;
    
    printLnB(bi1);
    printLnB(bi2);
    printLnB(bs1);
    printLnB(bs2);
    printLnB(bb1);
}
