# Excercise 2.1
## 1 Evaluate
### Applicative-order evaluation
```
minimum [2,7,1,9,6,5]
= head (insertionSort [2,7,1,9,6,5])
= head (insert 2 (insertionSort [7,1,9,6,5])
= head (insert 2 (insert 7 (insertionSort [1,9,6,5])))
= head (insert 2 (insert 7 ... insertionSort[])...)
= head (insert 2 (insert 7 ... insert 5 [])...)
= head (insert 2 (insert 7 ... insert 6 (5:[])...)
= head (insert 2 (insert 7 ... (5 : insert 6 [])...)
= head (insert 2 (insert 7 ... insert 9 (5 : 6 : [])...)
= head (insert 2 (insert 7 ... (5 : insert 9 (6 : []))...)
= head (insert 2 (insert 7 ... (5 : 6 : (insert 9 []))...)
= head (insert 2 (insert 7 ... (5 : 6 : 9 : [])...)
= head (insert 2 (insert 7 (insert 1 (5 : 6 : 9 : []))))
= head (insert 2 (insert 7 (1 : 5 : 6 : 9 : [])))
= head (insert 2 (1 : (insert 7 (5 : 6 : 9 : [])))
= head (insert 2 (1 : 5 : (insert 7 (6 : 9 : [])))
= head (insert 2 (1 : 5 : 6 : (insert 7 (9 : [])))
= head (insert 2 (1 : 5 : 6 : 7 : 9 : []))
= head (1 : (insert 2 (5 : 6 : 7 : 9 : [])))
= head (1 : 2 : 5 : 6 : 7 : 9 : [])
= head (1 : 2 : 5 : 6 : 7 : 9 : [])
= 1

```
### Normal-order evaluation

```
minimum [2,7,1,9,6,5]
= head (insertionSort [2,7,1,9,6,5])
= head (insert 2 (insertionSort [7,1,9,6,5])
= head (insert 2 (insert 7 (insertionSort [1,9,6,5])))
= head (insert 2 (insert 7 ... insertionSort[])...)
= head (insert 2 (insert 7 ... insert 5 [])...)
= head (insert 2 (insert 7 ... insert 6 (5:[])...)
= head (insert 2 (insert 7 ... insert 9 (5 : insert 6 [])...)
= head (insert 2 (insert 7 ... insert 1 (5 : (insert 9 (insert 6 [])))))
= head (insert 2 (insert 7 ... insert 1 (5 : (insert 9 (insert 6 [])))))
= 







```

## 2 Running time

Applicative-order : O(n^2)
Normal-order : O(n)
