#include <stdio.h>
#include <stdlib.h>

int main()
{
	FILE *fp;
	char path[100];

	fp = popen("ls *", "r");
	while (fgets(path, 100, fp) != NULL)
	    printf("%s", path);
	pclose(fp);

	exit(0);

}
