

fun int main()
{
    ~(int readonly a, (int, string)[] b) = ~(1, [~(5, "")]);
    b[4] = ~(1, "aa");
    b[5] = ~(1, ["aa"]);
}