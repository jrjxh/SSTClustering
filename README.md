SSTClustering
========
  在商业银行日常经营管理中，经常会产生大量非结构性文本数据，如何对这些文本数据进行分析和挖掘，从中提炼出有价值的信息并加以有效应用，已经成为大数据时代商业银行需要解决的一项重要课题。通过构建“半监督”文本聚类技术，对文本主题、类别、关键字词和数据样本之间的关系进行学习，从而实现对非结构性信息的结构化转换，相关应用对商业银行经营管理提升起到积极推动作用。


# SSTClustering的安装(Installation)
SSTClustering包的源代码在[Github/jrjxh/SSTClustering](https://github.com/jrjxh/SSTClustering)上提供下载与安装，安装需要使用以下代码：

```s
require(devtools)
devtools::install_github('jrjxh/SSTClustering')
```
# 使用方法
```s
#install.package("hash")
#install.package("jiebaR")
#install.package("stringr")
library(SSTClustering) 
sstcjob()
```
# 依赖的第三方包 

- hash

- jiebaR

- stringr


