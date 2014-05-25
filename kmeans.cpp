/****************************************
kmeans实现
****************************************/
#include <vector>
typedef std::vector<std::vector<float> > Matrix;

/****************************************
对@data进行kmeans聚类
@T 为数据类型，能够使用T[][]直接访问数据，且原子数据类型为float
@k 聚类划分的簇的个数
@bestlabels 调用函数时若T为空则使用随机产生的初始点，
   否则使用 @bestLabels提供的数据作为初始点;
   聚类完成之后bestLabels标记每个特征所属的簇，所以有@k行
@maxr 为最大迭代次数，当迭代次数超过@maxr时结束算法
@presision 为最少的精度,每次更新前后的中心点的距离小于该精度时结束算法
@centers 为聚类后的簇的中心点
****************************************/
template<class T>
void kmeans(const T &data, int k, T &bestLabels,
            int maxr, float precision, T &centers);

/****************************************
根据簇中心更新每个簇的元素
@data为要更新的数据
@bestLabels 为更新后的数据
@centers 数据的簇中心
****************************************/
template<class T>
void updateClusters(const T &data, T &bestLabels, const T &centers);

/****************************************
根据簇@bestLabels计算每个簇的中心
****************************************/
template<class T>
void updateCenters(const T &data,const T &bestLabels, T &centers);

//生成随机的初始点
template<class T>
void randomInit(const T& data, int k, T &centers);

//Within-cluster sum of squares
void wcss();

//kmeans定义
template<class T>
void kmeans(const T &data, int k, T &bestLabels,int maxr, float precision, T &centers){
  if(bestLabels.empty()) //没有提供初始点
    randomInit(data,k,centers); //使用随机化的初始点
  int i=0; //迭代次数
  T oldCenters; //更新前的簇中心点
  T oldLabels;  //更新前的簇元素标记
  bool isTerm = false; //算法收敛
  float curf; //精度
  do{
    //    oldLabels = be
    updateClusters(data,bestLabels,centers);
    updateCenters(data,bestLabels,centers);
    curf = wcss(data,bestLabels, centers);
    //    if(term
    i++;
  }while( i <= maxr || curf <= precision);
}
