## BUAA_2020fall_compiler

### 项目概述

本项目是使用C++语言编写的编译器，能够将[类C文法](doc/文法说明.html)编译为MIPS汇编代码。

除了基本的循环、分支语句外，编译器还实现了函数调用、数组存取等功能，能够实现函数的递归调用、参数中调用函数等高级功能，并有一定的错误处理能力。

### 使用说明

* 源文件：`complier.cpp`
* input：`testfile.txt`
* output
  * 报错信息：`error.txt`
  * 词法、语法分析结果：`test.txt`
  * 中间代码：`mCode.csv`
  * 生成代码：`mips.txt`

生成的MIPS代码可借助 `Mars.jar` 运行测试。

### 项目设计

[项目详细设计文档](doc/编译原理课程设计总文档.md)

### 相关规范

[文法规范](doc/文法说明.html)

[词法定义](doc/词法定义.md)

[错误处理定义](doc/错误类型定义.md)

