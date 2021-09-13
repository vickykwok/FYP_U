DATA temp1;
    INPUT Hour CO2 Traffic Wind;
cards;
1      2.05       52.20     -0.22
2      1.65       30.72      0.00
3      1.46       14.32      0.00
4      1.10        9.35      0.00
5      1.03        9.85      0.09
6      2.16       44.91     -0.10
7      3.19      133.12     -0.10
8      5.67      227.30     -0.18
9      5.58      246.81      0.19
10     5.55      203.24      1.17
11     5.65      214.07      2.22
12     5.15      200.33      3.19
13     6.08      205.26      4.29
14     4.74      167.41      5.66
15     6.41      202.71      6.23
16     7.96      285.33      6.50
17     7.22      255.01      4.94
18     6.70      341.68      4.20
19     6.98      220.75      3.72
20     4.07      192.06      2.37
21     3.19      133.09      1.39
22     3.83      140.92      0.50
23     2.94      107.45      0.44
24     2.75       87.79      0.08
;

*******************************************************************;
*                                                                 *;
*  Multiple Linear Regression Model with Linear Predictors Only:  *;
*                                                                 *;
*  Hour, Traffic & Wind                                           *;
*                                                                 *;
*******************************************************************;


Proc STEPWISE data = temp1;
     model CO2 = Hour Traffic Wind;
     Title1 'Stepwise Regression with Linear Predictors: Hour, Traffic & Wind';
     Title2 '================================================================';



Proc Reg data = temp1;
     model CO2 = Traffic Wind;
     Title1 'Regression Model with Significant Linear Predictors: Traffic & Wind';
     Title2 '===================================================================';



Data temp2;
     set temp1;
      pi = 3.1415926;
      H1 = Hour;
      H2 = Hour**2;
      H3 = Hour**3;
      T1 = Traffic;
      T2 = Traffic**2;
      T3 = Traffic**3;
      W1 = Wind;
      W2 = Wind**2;
      W3 = Wind**3;
      S2 = sin((2*pi/24)*Hour);
      C2 = cos((2*pi/24)*Hour);
      S4 = sin((4*pi/24)*Hour);
      C4 = cos((4*pi/24)*Hour);
      S6 = sin((6*pi/24)*Hour);
      C6 = cos((6*pi/24)*Hour);
      S8 = sin((8*pi/24)*Hour);
      C8 = cos((8*pi/24)*Hour);
     S10 = sin((10*pi/24)*Hour);
     C10 = cos((10*pi/24)*Hour);
     S12 = sin((12*pi/24)*Hour);
     C12 = cos((12*pi/24)*Hour);
     S14 = sin((14*pi/24)*Hour);
     C14 = cos((14*pi/24)*Hour);
     S16 = sin((16*pi/24)*Hour);
     C16 = cos((16*pi/24)*Hour);
     S18 = sin((18*pi/24)*Hour);
     C18 = cos((18*pi/24)*Hour);
     S20 = sin((20*pi/24)*Hour);
     C20 = cos((20*pi/24)*Hour);
     S22 = sin((22*pi/24)*Hour);
     C22 = cos((22*pi/24)*Hour);
     S24 = sin((24*pi/24)*Hour);
     C24 = cos((24*pi/24)*Hour);
     S26 = sin((26*pi/24)*Hour);
     C26 = cos((26*pi/24)*Hour);

Proc print data=temp2;
	Title1 'temp2';
    Title2 '=====';
Run;

******************************************************************;
*                                                                *;
*  Stepwise Regression of ALL Predictors and Harmonics Selected  *;
*                                                                *;
******************************************************************;


Proc STEPWISE data = temp2;
     Model CO2 = H1 - H3 T1 - T3 W1 - W3 S2 C2 S4 C4 S6 C6 S8 C8 S10 C10 S12 C12 S14 C14 S16 C16 S18 C18 S20 C20;
     Title 'Stepwise Regression of CO2 Data';
Run;



/**************************************************************************/
/*                                                                        */
/* Remove those insignificant predictors                                  */
/*                                                                        */
/* Re-build a regression model with those sig. variables in the model     */
/*                                                                        */
/**************************************************************************/


Proc reg data = temp2;
     model CO2 = T2 T3 W2 S6 C6 C8 C12 S16 C18;
     Title1 'Final Regression Model for CO2 Data';
     Title2 '===================================';
Run;
