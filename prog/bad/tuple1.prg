fun void main()
{
    (int, string, bool) t1;
    ~(int readonly a, string readonly b, bool readonly c) = t1;

    ((int, string, bool)[], string) t2 = ~([t1, t1], ~("aaa", 5));
}