#include<stdio.h>

long factorial(long n){
	int rest = 1;	
	for (int i = 1 ; i < n; ++i) {
		rest *= i;
	}

	return res;
}

int main(int argc, char* argv){
	printf("n = ");
	int n;
	scanf("%d", &n);
	printf("%d \n", factorial(n));

return 0;
}
