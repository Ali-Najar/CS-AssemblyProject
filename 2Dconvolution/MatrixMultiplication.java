//Ali Najar 401102701

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;

public class MatrixMultiplication {


    public static void main(String[] args) throws FileNotFoundException {
        int n;
        Scanner scanner = new Scanner(System.in);
        n = scanner.nextInt();
        PrintWriter writer1 = new PrintWriter("output1.txt");
        float[][] array1 = new float[n][n];
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++){
                array1[i][j] = i+j;
                writer1.print((i + j) + " ");
            }
            writer1.println();
        }
        writer1.close();
        PrintWriter writer2 = new PrintWriter("output2.txt");
        float[][] array2 = new float[n][n];
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++) {
                array2[i][j] = j-i;
                writer2.print((j - i) + " ");
            }
            writer2.println();
        }
        writer2.close();
        float[][] result = new float[n][n];
        System.out.println("matrix multiplication time:");
        long start = System.nanoTime();
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++) {
                float sum = 0;
                for (int k = 0; k < n; k++)
                    sum += array1[i][k] * array1[k][j];
                result[i][j] = sum;
            }
        long end = System.nanoTime();
        System.out.println((end-start));
        float sum = 0;
        System.out.println("matrix dot product time:");
        start = System.nanoTime();
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++) {
                sum+= array1[i][j] * array2[i][j];
            }
        end = System.nanoTime();
        System.out.println((end-start));
    }
}
