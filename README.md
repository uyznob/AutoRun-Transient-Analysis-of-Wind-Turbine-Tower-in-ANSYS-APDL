# AutoRun-Transient-Analysis-of-a-Wind-Turbine-Tower-in-ANSYS-APDL
The FORTRAN-base scripts instruct ANSYS-APDL to run the transient simulation for a specific wind turbine tower. All of dynamic data including natural frequency, mode shapes and acceleration at several location along the tower are exported to a complete set of data. The dataset is employed for the input layer of an Artificial Neural Networks.

A. The details could be seen in the following conference paper for the congress "The 13th International Workshop on Advanced Smart Materials and Smart Structures Technology" at University of Tokyo, Japan 2017. Link: https://www.researchgate.net/publication/318815954_Damage_Identification_of_Wind-Turbine_Tower_using_Modal_Properties-Based_Artificial_Neural_Networks

B. The main script is WWT*.mac, which is actually the input file of ANSYS APDL. The critical note is that this file is coded by FORTRAN so that it can build the geometry and mesh of a Wind Turbine Model and then do multiple computation with different damage scenarios. The computation could last for 1-2 hours without any interference of user. The dynamic data is exported corresponding by several subsripts EXP*.
