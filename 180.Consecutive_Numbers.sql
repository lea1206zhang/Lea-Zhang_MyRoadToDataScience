SELECT  DISTINCT L1.Num AS ConsecutiveNums
FROM    Logs AS L1
JOIN    Logs AS L2
ON      L1.Id = L2.Id -1
JOIN    Logs As L3
ON      L2.Id = L3.Id -1
WHERE   L1.Num = L2.Num AND L2.Num = L3.Num;
