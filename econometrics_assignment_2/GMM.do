
***Panel Data analysis***

* GMM**
xtset Individual time, generic

xtabond2 charity l.charity price ms income deps age, ///
gmm(L2.(charity l.charity price ms income deps age))
