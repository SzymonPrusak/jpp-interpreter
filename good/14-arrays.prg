

fun void printArr(int[] readonly a, int readonly n)
{
    for (i = 0 to n - 1)
        printLnI(a[i]);
}


fun void main()
{
    int[] arr1 = new int[100];
    for (i = 0 to 99)
        arr1[i] = i;

    int[] readonly arr2 = [1,2,3,4,5,6,7,8,9,10];

    printArr(arr1, 100);
    printArr(arr2, 10);
}