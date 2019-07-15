#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <float.h>
#include <stdbool.h>

#define L 150
#define C 4
#define MAXMIN 0.002

typedef struct Donnees Donnees1;
struct N_conf{
    Donnees1 *dataMat;
    int neuron_quant;
    int iteration_quant;
    int phase1;
    int phase2;
}N;
struct Donnees {
    char *Fleur;
    double *arrayF;
};
struct neuron {
    double *act;
    char *etiq;
    double *w;
};
typedef struct neuron t_node;
struct bmu {
    int l;
    int c;
    struct bmu *suiv;
};
typedef struct bmu t_bmu;
struct list_bmu {
    int size;
    t_bmu *cbmu;
};
typedef struct list_bmu t_ListBmu;
struct network {
    double alpha;
    int rv;
    t_node **node;
    double *capteur;
};
typedef struct network t_net;

// To read from the file
double **Lfichier(char *path) {
    double **Matrice = (double **)malloc(L * sizeof(double*));
    int i;
    for(i = 0;i<L; i++)
    {
        Matrice[i] = (double*)malloc(C * sizeof(double));
    }
    FILE *file;
    file = fopen(path,"r");
    char *line = malloc(256 * sizeof(char));
    i = 0;
    while(fgets(line, 256, file) != NULL)
    {
        sscanf(line, "%lf,%lf,%lf,%lf", &Matrice[i][0],&Matrice[i][1], &Matrice[i][2], &Matrice[i][3]);
        i++;
    }
    fclose(file);
    return Matrice;
}
int nhddehors(int n,int m,int i,int j,int r){
if((i-r)>=0 && (j-r)>=0 && (i+r)<=n && (j+r)<=n && (i+r)<=m && (j+r)<=m) 
return 1;
return 0;
}
double* AllocVecteur(int size){
    double *res;
    res = (double *) calloc(size,sizeof(double));
    return res;
}
double** AllocMatrice(int l,int c) {
    double **mat;
    int i,j;
    mat = (double **) malloc (c*sizeof(double*));
	for (i=0;i<c;i++) {
		mat[i] = (double *) malloc (l*sizeof(double));
	}
	for(i=0;i<l;i++)
	   for(j=0;j<c;j++)
		mat[i][j]=0.0;
    return mat;
}
double DisEuc(double x[],double w[]){

   double res=0.0; int i;
   for( i=0;i<C;i++)
   res+=(x[i]-w[i])*(x[i]-w[i]);
   return sqrt(res);
}
Donnees1* VectDon(int size) {
    Donnees1 *res;
    res = (Donnees1*) malloc(L*sizeof(Donnees1));
    return res;
}
// create matrice function
double** MatrNorm(double **matr) {
    int i,j;
    double sum=0.0,temp;
    for (i=0;i<L;i++) {
        for (j=0;j<C;j++) {
            sum+=matr[i][j]*matr[i][j];
        }
        temp=sqrt(sum);
        for (j=0;j<C;j++) {
            matr[i][j]=matr[i][j]/temp;
        }
    }
    return matr;
}
double* moyenne(double **x,int length,int width){
     int i,j;
     double *ans=AllocVecteur(C),temp=0.0;
       for(j=0;j<length;j++){
	for(i=0;i<width;i++)
	 temp+=x[i][j];
	 ans[i]=temp/length;
	 temp=0.0;
        }
     return ans;
}
//initialization
Donnees1* InitiaDon(Donnees1 *MatriceDon,double **ans) {
    int i;
    for (i=0;i<L;i++){
        MatriceDon[i].arrayF=(double*) malloc(4*sizeof(double));
        MatriceDon[i].arrayF=ans[i];
        MatriceDon[i].Fleur=(char*) malloc(20*sizeof(char));
        if(i<50) {
            MatriceDon[i].Fleur="setosa";
        }
        else if(i>49&&i<100) {
            MatriceDon[i].Fleur="versicolor";
        }
        else {
            MatriceDon[i].Fleur="virginica";
        }
    }
    return MatriceDon;
}
t_node** initializeNodes(int a,int b) {
    int i;
    t_node **nodes;
    nodes = (t_node**) malloc(a*sizeof(t_node*));
    for (i=0;i<a;i++) {
        nodes[i]=(t_node*) malloc(b*sizeof(t_node));
    }
    return nodes;
}
t_node** fillNeurons(t_node **nodes,double *array,int a,int b) {
    int i,j,k;
    double min,max;
    for (i=0;i<a;i++) {
        for(j=0;j<b;j++) {
            nodes[i][j].w=AllocVecteur(C);
            for (k=0;k<C;k++) {
                min=array[k]-MAXMIN;
                max=array[k]+MAXMIN;
                nodes[i][j].w[k]=min+(double)rand()/RAND_MAX*(max-min);
            }
        }
    }
    return nodes;
}

//update function in this structure
double* Updateweight (double *w,double *x,int alpha,int a,int b,int l,int c,int r) {
    double *ans;
    int i;
    ans = (double*) malloc(C*sizeof(double));
    ans = w;
    if (nhddehors(a,b,l,c,r)) {
        for (i=0;i<C;i++) {
            ans[i]+=alpha*(x[i]-w[i]);
        }
    }
    return ans;
}

// shuffle fuction Donnees1
Donnees1* Shuffle(Donnees1* MatriceDon) {
    int i,alea,temp;
    char *fname;
    double *arr;
    arr=AllocVecteur(C);
    for (i=0;i<L;i++) {
        temp=L-i;
        alea=rand()%temp+i;
        arr=MatriceDon[i].arrayF;
        fname=MatriceDon[i].Fleur;
        MatriceDon[i].arrayF=MatriceDon[alea].arrayF;
        MatriceDon[i].Fleur=MatriceDon[alea].Fleur;
        MatriceDon[alea].arrayF=arr;
        MatriceDon[alea].Fleur=fname;
    }
    return MatriceDon;
}
//Learn algorithm

t_ListBmu* Findbmu (t_net *reseau,int a,int b) {
    t_ListBmu *res;
    res = (t_ListBmu*) malloc(sizeof(t_ListBmu));
    int i,j,k;
    t_bmu *tempBmu,*tmpBM;
    tempBmu = (t_bmu*) malloc(sizeof(t_bmu));
    tmpBM = (t_bmu*) malloc(sizeof(t_bmu));
    tempBmu->suiv=NULL;
    double minval=10.0,tmper;
    int x,y;
    for (i=0;i<a;i++) {
        for (j=0;j<b;j++) {
            tmper=DisEuc(reseau->node[i][j].w,reseau->capteur);
            if(minval>tmper) {
                minval=tmper;
                x=j;
                y=i;
                tempBmu->l=x;
                tempBmu->c=y;
                tempBmu->suiv=NULL;
                res->size=1;
                res->cbmu=tempBmu;
            }
            else if (minval==tmper) {
                x=j;
                y=i;
                tempBmu->l=x;
                tempBmu->c=y;
                tempBmu->suiv=NULL;
                tmpBM=res->cbmu;
                for (k=1;k<res->size;i++) {
                    tmpBM=tmpBM->suiv;
                }
                tmpBM->suiv=tempBmu;
            }
        }
    }
    return res;
}
int main()
{
    int i,j,a,b,ind1,ind2;
    double **res,*VectMoyenne;
    Donnees1 *MatriceDon;
    t_net *reseau;
    t_ListBmu *listBmu;
    res=Lfichier("iris.txt");
    res=MatrNorm(res);
    VectMoyenne=moyenne(res,L,C);
    MatriceDon = VectDon(L);
    MatriceDon = InitiaDon(MatriceDon,res);
    j=(int)sqrt(L);
    i=5*j;
    N.neuron_quant=i;
    N.iteration_quant=5000;//=500*L;
    i=N.iteration_quant;
    j=i/5;
    i=i-j;
    N.phase1=j;
    N.phase2=i;
    a=5;
    b=12;
    //printf("a %d  b %d\n",a,b);
    reseau=(t_net*) malloc(sizeof(t_net));
    reseau->node=initializeNodes(a,b);
    reseau->node=fillNeurons(reseau->node,VectMoyenne,a,b);
    reseau->alpha=0.7;
    reseau->rv=3;

    listBmu=(t_ListBmu*) malloc(sizeof(t_ListBmu));
    reseau->capteur=AllocVecteur(C);
    t_bmu *tempBmu;
    tempBmu=(t_bmu*) malloc(sizeof(tempBmu));
    for (i=0;i<N.phase1;i++) {
        int prof=N.phase1/reseau->rv;
        if(i!=0&&i%prof==0) {
            reseau->rv--;
        }
        MatriceDon=Shuffle(MatriceDon);
        for (j=0;j<L;j++) {
            reseau->capteur=MatriceDon[j].arrayF;
            listBmu=Findbmu(reseau,a,b);
            int bmuloc1 = rand()%listBmu->size;
            tempBmu=listBmu->cbmu;
            for (ind1=1;ind1<bmuloc1;ind1++) {
                tempBmu=tempBmu->suiv;
            }
            for (ind1=0;ind1<a;ind1++) {
                for (ind2=0;ind2<b;ind2++) {
                    reseau->node[ind1][ind2].w=Updateweight(reseau->node[ind1][ind2].w,reseau->node[tempBmu->c][tempBmu->l].w,reseau->alpha,a,b,tempBmu->l,tempBmu->c,reseau->rv);
                }
            }
        }
     }
    reseau->alpha/=10;
    for (i=0;i<N.phase2;i++) {
        MatriceDon=Shuffle(MatriceDon);
        for (j=0;j<L;j++) {
            reseau->capteur=MatriceDon[j].arrayF;
            listBmu=Findbmu(reseau,a,b);
            int bmuloc2 = rand()%listBmu->size;
            tempBmu=listBmu->cbmu;
            for (ind1=1;ind1<bmuloc2;ind1++) {
                tempBmu=tempBmu->suiv;
            }
            reseau->node[tempBmu->c][tempBmu->l].etiq=MatriceDon[i].Fleur;
            for (ind1=0;ind1<a;ind1++) {
                for (ind2=0;ind2<b;ind2++) {
                    reseau->node[ind1][ind2].w=Updateweight(reseau->node[ind1][ind2].w,reseau->node[tempBmu->c][tempBmu->l].w,tempBmu->l,tempBmu->c,reseau->alpha,a,b,reseau->rv);
                }
            }
        }
    }

			

    return 0;
}
