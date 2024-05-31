//Ali Najar 401102701

import java.util.Scanner;

import static java.lang.Math.*;

public class Convolution {
    static int edgeCalculator(int a, int n){
        return min(max(a,0),n-1);
    }

    static int mirrorEdge(int a,int n){
        if(a<0) return abs(a)-1;
        if(a>n-1) return 2*n-a-1;
        return a;
    }

    static void convolutionAsMatrixMul(float[][] res,float[][] M,float[][] kernel,float[][] mat,int matSize,int kernelSize){
        int t = matSize - kernelSize + 1;
        for(int i=0;i<t*t;i++)
            for(int j=0;j<matSize*matSize;j++)
                M[i][j] = 0;
        for(int i=0;i<t;i++){
            for(int j=0;j<t;j++){
                for(int k=0;k<kernelSize;k++){
                    for(int h=0;h<kernelSize;h++){
                        M[i*t+j][i*matSize+k*matSize+j+h] = kernel[k][h];
                    }
                }
            }
        }
        for(int i=0;i<t;i++){
            for(int j=0;j<t;j++){
                float sum = 0;
                for(int k=0;k<matSize*matSize;k++){
                    sum += M[i*t+j][k]*mat[k/matSize][k%matSize];
                }
                res[i][j] = sum;
            }
        }
    }
    static void ordinaryConvolution(float[][] res,float[][] kernel,float[][] mat,int matSize, int kernelSize){
        for(int i=0;i<matSize-kernelSize+1;i++){
            for(int j=0;j<matSize-kernelSize+1;j++){
                float sum = 0;
                for(int k=0;k<kernelSize;k++){
                    for(int t=0;t<kernelSize;t++){
                        sum += kernel[k][t] * mat[i+k][j+t];
                    }
                }
                res[i][j] = sum;
            }
        }
    }

    static void extendedMatrixConvolution(float[][] res, float[][] kernel, float[][] mat, int matSize, int kernelSize){
        for(int i=0;i<matSize;i++){
            for(int j=0;j<matSize;j++){
                float sum = 0;
                for(int k=0;k<kernelSize;k++){
                    for(int t=0;t<kernelSize;t++){
                        sum += kernel[k][t] * mat[edgeCalculator(i+k-kernelSize/2,matSize)][edgeCalculator(j+t-kernelSize/2,matSize)];
                    }
                }
                res[i][j] = sum;
            }
        }
    }

    static void mirroredMatrixConvolution(float[][] res, float[][] kernel, float[][] mat, int matSize, int kernelSize){
        for(int i=0;i<matSize;i++){
            for(int j=0;j<matSize;j++){
                float sum = 0;
                for(int k=0;k<kernelSize;k++){
                    for(int t=0;t<kernelSize;t++){
                        sum += kernel[k][t] * mat[mirrorEdge(i+k-kernelSize/2,matSize)][mirrorEdge(j+t-kernelSize/2,matSize)];
                    }
                }
                res[i][j] = sum;
            }
        }
    }
    static void zeroEdgeMatrixConvolution(float[][] res, float[][] kernel, float[][] mat, int matSize, int kernelSize){
        for(int i=0;i<matSize;i++){
            for(int j=0;j<matSize;j++){
                float sum = 0;
                for(int k=0;k<kernelSize;k++){
                    for(int t=0;t<kernelSize;t++){
                        if(i+k-kernelSize/2>=0 && i+k-kernelSize/2<=matSize-1 && j+t-kernelSize/2>=0 && j+t-kernelSize/2<=matSize-1)
                            sum += kernel[k][t] * mat[i+k-kernelSize/2][j+t-kernelSize/2];
                    }
                }
                res[i][j] = sum;
            }
        }
    }

    public static void main(String[] args) {
        int n;
        Scanner scanner = new Scanner(System.in);
        int matSize,kernelSize;
        kernelSize = scanner.nextInt();
        matSize = scanner.nextInt();
        float[][] kernel = new float[kernelSize][kernelSize];
        float[][] mat = new float[matSize][matSize];
        float[][] res = new float[matSize][matSize];
        long start = 0;
        long end = 0;
        for (int i = 0; i < kernelSize; i++) for (int j = 0; j < kernelSize; j++) kernel[i][j] = scanner.nextFloat();
        for (int i = 0; i < matSize; i++) for (int j = 0; j < matSize; j++) mat[i][j] = scanner.nextFloat();
        start = System.nanoTime();
        ordinaryConvolution(res,kernel,mat,matSize,kernelSize);
        end = System.nanoTime();
        System.out.println(end-start);
        System.out.println("ordinary");
//        for (int i = 0; i < matSize-kernelSize+1; i++){
//            for (int j = 0; j < matSize-kernelSize+1; j++) System.out.print(res[i][j]+" ");
//            System.out.println();
//        }
        System.out.println();
        start = System.nanoTime();
        extendedMatrixConvolution(res,kernel,mat,matSize,kernelSize);
        end = System.nanoTime();
        System.out.println(end-start);
        System.out.println("extended");
//        for (int i = 0; i < matSize; i++){
//            for (int j = 0; j < matSize; j++) System.out.print(res[i][j]+" ");
//            System.out.println();
//        }
        System.out.println();
        start = System.nanoTime();
        mirroredMatrixConvolution(res,kernel,mat,matSize,kernelSize);
        end = System.nanoTime();
        System.out.println(end-start);
        System.out.println("mirrored");
//        for (int i = 0; i < matSize; i++){
//            for (int j = 0; j < matSize; j++) System.out.print(res[i][j]+" ");
//            System.out.println();
//        }
        System.out.println();
        start = System.nanoTime();
        zeroEdgeMatrixConvolution(res,kernel,mat,matSize,kernelSize);
        end = System.nanoTime();
        System.out.println(end-start);
        System.out.println("zero");
//        for (int i = 0; i < matSize; i++){
//            for (int j = 0; j < matSize; j++) System.out.print(res[i][j]+" ");
//            System.out.println();
//        }
        int t = matSize - kernelSize + 1;
        float[][] M = new float[t*t][matSize*matSize];
        System.out.println();
        start = System.nanoTime();
        convolutionAsMatrixMul(res,M,kernel,mat,matSize,kernelSize);
        end = System.nanoTime();
        System.out.println(end-start);
        System.out.println("convolution using matrix multiplication");
//        for (int i = 0; i < matSize-kernelSize+1; i++){
//            for (int j = 0; j < matSize-kernelSize+1; j++) System.out.print(res[i][j]+" ");
//            System.out.println();
//        }
    }
}
