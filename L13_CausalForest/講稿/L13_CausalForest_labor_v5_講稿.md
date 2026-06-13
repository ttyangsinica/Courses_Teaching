---
title: "L13 Causal Forest 講稿（v5）"
subtitle: "Causal Machine Learning (III): Causal Forest"
author: "Prof. Tzu-Ting Yang"
date: "2026-05-13"
lang: zh-TW
---

# L13 Causal Forest 講稿（v5）

這份講稿對應 `L13_CausalForest_labor_v5.tex`。目標不是逐字唸投影片，而是提供一個在課堂上可以流暢講解的版本：先從為什麼 ATE 不夠出發，一步步建立 decision tree、random forest、causal tree、causal forest 的直覺，最後示範 R code。

---

# 第一部分：Main Idea

## 【投影片】標題頁

各位好。這一講是 causal machine learning 系列的第三堂課，主題是 causal forest。前面兩講分別是 LASSO 和 double machine learning，今天的 causal forest 是這個系列的壓軸。

---

## 【投影片】Treatment Effect Heterogeneity

前面幾講我們花了很多時間估計 ATE，也就是：

$$\alpha_{\text{ATE}} = \mathrm{E}[Y_i^1 - Y_i^0]$$

這個數字代表整個 population 平均來說，treatment 的效果有多大。ATE 很好用，它告訴我們「這個政策平均來說有沒有效」。

但是，ATE 有一個根本的限制：它把所有人的異質性壓縮成一個數字。而實際上，treatment effect 可能因人而異。

以職業訓練為例。一個年輕、低教育程度的工人，訓練可能對他的幫助非常大；但一個有豐富經驗的中年工人，訓練對他的邊際效果可能很小。如果我們只看 ATE，這兩群人的效果就被平均掉了。

Causal forest 要估計的是 CATE：

$$\tau(x) = \mathrm{E}[Y_i^1 - Y_i^0 \mid X_i = x]$$

也就是：對於特徵為 $x$ 的人，treatment effect 平均是多少。這讓我們可以問一個更細的政策問題：誰受益最多？

---

## 【投影片】Motivating Example: Job Training

今天的例子就是政府的職業訓練計畫。

- Outcome $Y_i$：訓練後的收入
- Treatment $D_i$：有沒有參加訓練
- Covariates $X_i$：年齡、教育程度、種族、婚姻狀態、過去收入

傳統的政策評估問：訓練計畫平均來說有效嗎？這是 ATE 或 ATT 的問題。

Causal forest 要問的是：這個訓練計畫對哪些人特別有效？哪些人幾乎沒有幫助？

---

## 【投影片】Why Heterogeneity Matters（表格）

投影片上這個表格是一個簡單的例子。

- 年輕、低教育程度的工人：訓練讓收入從 8,000 增加到 11,000，效果是 3,000 美元。
- 年長、有豐富經驗的工人：收入從 18,000 增加到 18,500，效果只有 500 美元。
- 與社會脫節的工人（disconnected worker）：效果最大，4,000 美元。

如果我們只看平均，可能會說這個計畫平均效果是 2,500 美元，結論是「有效」。但這掩蓋了一個很重要的資訊：第三群工人的效果是第二群的八倍。

如果政府的資源有限，知道這一點非常重要。Causal forest 的輸出就是要告訴我們：**哪些人應該被優先服務？**

---

## 【投影片】Why Heterogeneity Matters（政策問題）

這裡列了幾個典型的勞動經濟學例子，說明為什麼 heterogeneity 在政策上很重要：

- **青年就業計畫**：哪些青年受益最多？是教育程度較低的，還是家庭背景較弱的？
- **教育政策**：教育投資的報酬，是否因學生背景或地區勞動市場不同而有差異？
- **最低工資**：最低工資的就業效果，是否因勞工技術、廠商規模或地區而不同？
- **育嬰假**：育嬰假政策的效果，是否因收入水準或職業類型而有差異？

這四個例子都是用 CATE 可以回答、但用 ATE 無法充分回答的問題。

---

## 【投影片】Treatment Effect Heterogeneity: Traditional Approaches

在 causal forest 出現之前，研究者探索 heterogeneity 的方法主要有兩種。

第一種是**理論驅動的分組分析（theory-based subgroup analysis）**。做法是根據經濟理論預先決定要按哪個 $X$ 來分組，例如分男女、分年齡組、分教育程度，然後在每個 subgroup 裡面分別估計 ATE。這個方法直觀，但問題是研究者必須事先知道「哪個 $X$ 重要」。如果猜錯了，就看不到真正的 heterogeneity。

第二種是**迴歸的交乘項（interaction terms）**：

$$Y_i = \alpha + \beta D_i + \gamma X_i + \delta (D_i \times X_i) + \varepsilon_i$$

係數 $\delta$ 捕捉 $D_i$ 的效果如何隨 $X_i$ 變化。這個方法也很常見，但它假設 heterogeneity 是**線性**的——也就是說，效果隨 $X$ 的變化是直線的、是 parametric 的形式。

這兩種傳統方法都有同一個缺點：研究者必須**事先指定**哪個 $X$ 重要，並且假設一個**線性、parametric 的異質性形式**。

---

## 【投影片】Causal Forest: A Data-Driven Approach（第一張）

Causal forest 的出發點是：我不想事先指定 heterogeneity 的形式。我要讓**資料告訴我**哪個 $X$ 重要、效果如何隨 $X$ 變化。

直觀來說，causal forest 的流程是：

$$\text{Data } (Y, D, X) \;\longrightarrow\; \text{Many honest causal trees} \;\longrightarrow\; \text{CATE } \hat{\tau}(x)$$

它把 random forest 的概念延伸到因果推論的脈絡下。每一棵樹用一個隨機的 subsample 來估計 CATE；然後把很多棵樹的估計平均起來，得到穩定的 $\hat{\tau}(x)$。

**Random subset 這一點很重要**：讓每棵樹看到不同版本的資料，可以讓各棵樹有所不同，平均之後才能消除噪音。

---

## 【投影片】Causal Forest: A Data-Driven Approach（第二張）

這一頁有一個非常重要的概念釐清：**Causal forest 不是一種識別策略（identification strategy）。**

它不解決 selection bias 的問題。它不是 RCT、DID 或 RDD。

Causal forest 是一種**彈性的 data-driven 估計量（estimator）**，用來估計 CATE。但要讓這些估計有因果意義，你需要搭配一個識別策略：

- 搭配 **RCT**：資料已經是隨機分配，CATE 就是「哪些人從隨機 treatment 中受益更多？」
- 搭配 **DID**：「treatment effect 是否因群體而有差異？」
- 搭配 **RDD**：「在 cutoff 附近，哪些人受益最多？」

和傳統方法相比，causal forest 的優點是：它自動從資料中搜尋哪些特徵驅動 heterogeneity，**不需要研究者事先指定**。

---

## 【投影片】How ATE and ATT Relate to CATE

一旦我們知道 CATE 函數 $\tau(x)$，ATE 和 ATT 其實只是它的不同加總：

$$\alpha_{\text{ATE}} = \mathrm{E}[\tau(X_i)], \qquad \alpha_{\text{ATT}} = \mathrm{E}[\tau(X_i) \mid D_i = 1]$$

所以 causal forest 並不是要**取代** ATE 或 ATT，而是估計一個更豐富的物件 $\tau(x)$，然後再從中還原 ATE 或 ATT。

另外，這也意味著：如果 heterogeneity 很小，$\tau(x) \approx \alpha_{\text{ATE}}$ 對所有 $x$ 都成立，causal forest 的輸出就會接近 ATE。

---

# 第二部分：Decision Tree and Random Forest

---

## 【投影片】Decision Trees: Main Idea

現在要從頭開始建立 decision tree 的直覺。因為 causal forest 是建立在 causal tree 上，causal tree 又是建立在 decision tree 上，所以這部分很基礎，但非常重要。

Decision tree 的核心想法是：把 covariate space 切成一塊一塊的矩形區域，在每一塊內用一個簡單模型（例如均值）來預測 $Y$。

演算法有三個步驟：
1. 找一個變數 $X^j$ 和切點 $s$，使得切割後的預測誤差最小。
2. 把資料分成兩個區域：$\{X^j < s\}$ 和 $\{X^j \geq s\}$。
3. 在每個區域內遞迴地重複這個過程，直到滿足停止條件。

最後，每個 **leaf（葉節點）** 裡的預測值，就是這個 leaf 裡所有觀測值的 $Y$ 的均值。

---

## 【投影片】Decision Trees: Finding the Best Split

那要怎麼找最好的切割點？

方法是計算 **RSS（residual sum of squares）**：

$$\text{RSS}(j, s) = \sum_{i \in R_1}(Y_i - \bar{Y}_{R_1})^2 + \sum_{i \in R_2}(Y_i - \bar{Y}_{R_2})^2$$

也就是：切割後，兩個區域內的殘差平方和加總。

我們的目標是找 $(j^*, s^*)$，讓 RSS 最小。這個思路類似 OLS 的直覺：RSS 小，代表切割後每個區域內的 $Y$ 更整齊、更容易預測。

找到最好的切割點之後，再對每個子區域遞迴執行同樣的步驟。

---

## 【投影片】Decision Trees: When to Stop Splitting（第一張）

如果我們一直切下去，最終每個 leaf 只有一個觀測值，RSS 降到零，但完全沒有預測能力——這是 overfitting。

所以需要**停止規則**來控制樹的複雜度：

- **Minimum leaf size（最小葉片大小）**：如果一個區域的觀測值數量小於 $k$（例如 $k = 5$），就不繼續切。這確保每個 leaf 有足夠的資料來可靠地估計均值。
- **Maximum tree depth（最大樹深）**：最多切 $d$ 層（例如 $d = 5$），leaf 的數量上限為 $2^d$。

---

## 【投影片】Decision Trees: When to Stop Splitting（第二張）

另一個停止規則：

- **Minimum RSS reduction（最小 RSS 降幅）**：如果最好的切割點只能讓 RSS 降低不到 $\varepsilon$，就停止。這跳過了「花很大力氣但改善很小」的切割。

為什麼必須有停止規則？一棵完全不停止的樹，會把每個觀測值都切成獨立的 leaf，完美擬合訓練資料，但對新資料的預測完全失效。這就是 **overfitting**。

所有的停止規則都是在控制「樹的彈性」和「過度擬合的風險」之間的 trade-off。

---

## 【投影片】Decision Trees: Illustration

用一個例子說明。假設我們要用年齡和教育程度來預測薪資 $Y$：

- **第一次切割**：age < 30 vs. age $\geq$ 30
- **第二次切割**（在 age $\geq$ 30 的子樣本內）：educ < 12 vs. educ $\geq$ 12

每個 leaf 包含「特徵相似、預測 $Y$ 也相似」的觀測值。這就是 decision tree 把 covariate space 分割成矩形區域的概念。

---

## 【投影片】Decision Trees: Tree Structure Example（樹狀結構圖）

投影片左側是樹的結構圖：

- **橘色節點（internal nodes）**：分裂規則，例如「age < 30？」
- **綠色葉節點（leaves）**：最終預測值，等於該 leaf 內所有觀測值的 $\bar{Y}$

右側的例子：一個年齡 35 歲、教育 14 年的新工人，走過「age $\geq$ 30」、「educ $\geq$ 12」兩個分支，落在 Leaf 3，預測薪資是 11,500 美元。

預測方式就是這麼簡單：順著樹的分支走，走到一個 leaf，就用那個 leaf 的均值當預測。

---

## 【投影片】Decision Trees: Partition of Feature Space（特徵空間分割圖）

右圖用二維平面來呈現 decision tree 的分割結果：

- 垂直線代表 age = 30 的切割
- 水平線（在 age $\geq$ 30 的區域）代表 educ = 12 的切割
- 三個矩形區域各自有一個預測值

重點：每個切割都必須是**矩形**的（axis-aligned），這是 decision tree 的結構限制。樹越深，區域越多，彈性越大，但也越容易 overfit。

---

## 【投影片】Why a Single Decision Tree Is Not Enough（導論）

Decision tree 有一個根本的弱點：**high variance（高變異數）**。

問題在哪裡？

- 樹會「記住」訓練資料，而不是學習真實的規律
- 資料稍微有一點變動——例如加入或移除幾個觀測值——可能導致整棵樹的結構完全不同
- 對新資料的預測因此很不穩定

---

## 【投影片】High Variance: Sample A vs. Sample B（對比表格）

用一個非常具體的例子來說明 high variance。

**Sample A**：5 個工人，age = {20, 25, 40, 45, 50}，educ = {9, 9, 9, 9, 12}，wage = {$8, $9, $21, $22, $23}。

注意到：age < 30 的工人薪資是 $8–$9，age $\geq$ 30 的是 $21–$23。所以樹很自然地用 **age < 30** 來分割。

**Sample B**：只改了一個工人——把第二個人（age=25, educ=9, wage=$9）換成（age=25, educ=12, wage=$30）。

現在 age < 30 的區域有 $8 和 $30，差異很大。用 age 切割的效果就很差了。樹反而發現：按 educ < 11 切割，低學歷組（$8, $21, $22）和高學歷組（$30, $23）的薪資更整齊。所以樹**改成用 Education 分割**。

只換了一個人，樹就完全換了一個分割變數。這就是 high variance。

---

## 【投影片】High Variance: RSS 計算表格

用 RSS 來量化這件事：

| 候選切割 | Sample A 的 RSS | Sample B 的 RSS |
|---|---|---|
| Age < 30 | **2.5** | 244 |
| Educ < 11 | 170 | **147** |

在 Sample A，age < 30 把薪資切得很整齊，RSS 只有 2.5；在 Sample B，同樣的切割讓 age < 30 組有 $8 和 $30，RSS 暴增到 244。

樹永遠選 RSS 最小的切割：換一個工人，age 切割的 RSS 從 2.5 爆到 244，而 educ 切割的 RSS 從 170 降到 147，所以樹就切換到 educ 了。

這讓我們看到：**只需要換一個觀測值，RSS 的排名就改變了，樹的結構也完全不同。** 這就是 high variance 的核心問題。

---

## 【投影片】High Variance: Tree A vs. Tree B（樹狀圖）

把這兩棵樹並排放：

- **Tree A**（based on Sample A）：root 是 age < 30，Yes → $8.5/hr，No → $22/hr
- **Tree B**（based on Sample B）：root 是 educ < 11，Yes → $17/hr，No → $26.5/hr

完全不同的兩棵樹，只因為訓練資料差一個工人。

---

## 【投影片】High Variance: 對新工人的預測

假設我們要預測一個新工人的薪資：age = 22，educ = 16。

- Tree A：age < 30 → Yes → 預測 **$8.5/hr**
- Tree B：educ < 11 → No（educ=16 $\geq$ 11）→ 預測 **$26.5/hr**

同樣的特徵，同樣的人，兩棵樹的預測差了 $18/hr。這就是 high variance 的實際代價：完全換了一個人訓練出來的樹，對新人的預測可以差到這麼多。

**解決方案**：不要只用一棵樹，而是種很多棵樹，每棵樹用不同的 subsample 訓練，然後平均它們的預測。這就是 **Random Forest**。

---

## 【投影片】Random Forest: Two Sources of Randomness（兩種隨機性）

Random Forest 讓各棵樹彼此不同的方法有兩種：

**1. Random subsampling（隨機抽樣）**：每棵樹只看全部資料的一個隨機子集。所以每棵樹訓練的資料不一樣，學到的分割也不一樣。

**2. Random feature selection（隨機特徵選取）**：在每次切割時，不是考慮所有變數，而是隨機抽取一部分變數（例如 $\sqrt{p}$ 個），從這個子集中找最好的切割。

這兩種隨機性讓每棵樹有所不同，確保平均之後的效果是真正的去噪，而不是把同樣的錯誤重複 $B$ 次。

---

## 【投影片】Random Forest: Algorithm

Random Forest 的演算法：

1. 從全部資料中隨機抽取一個 subsample
2. 在這個 subsample 上，每次切割時隨機選取一部分變數
3. 生長一棵完整的 decision tree（通常不剪枝）
4. 重複步驟 1–3，生長 $B$ 棵樹
5. 對新的 $x$，把所有樹的預測平均：

$$\hat{Y}(x) = \frac{1}{B}\sum_{b=1}^{B} \hat{Y}_b(x)$$

直覺：每棵樹都有噪音，但如果各棵樹的噪音是相互獨立的，平均後噪音就會消失，剩下的是訊號。

---

## 【投影片】Random Forest: Illustration（多棵樹示意圖）

投影片的圖示呈現了 Random Forest 的核心想法：

- 全部資料進去
- 每棵樹用不同的 subsample 和特徵子集訓練，學到不同的分割
- 對同一個 $x$，不同的樹可能走不同的路徑，落在不同的 leaf，給出不同的預測值
- 最後把所有樹的預測平均，得到更穩定的結果

---

# 第三部分：Causal Tree

---

## 【投影片】From Prediction Tree to Causal Tree（從預測樹到因果樹）

現在把 decision tree 的邏輯延伸到因果推論。

| | Prediction Tree | Causal Tree |
|---|---|---|
| **目標** | 預測 outcome $Y_i$ | 估計 treatment effect $\tau(x)$ |
| **切割規則** | 找讓 outcome level 最不同的分割 | 找讓 treatment effect 最不同的分割 |
| **葉節點的值** | $\hat{Y}(\ell) = \bar{Y}_\ell$ | $\hat{\tau}(\ell) = \bar{Y}^1_\ell - \bar{Y}^0_\ell$ |

**關鍵類比**：prediction tree 在找 outcome 的異質性；causal tree 在找 treatment effect 的異質性。

切割的邏輯一樣，但目標變了。

---

## 【投影片】Causal Tree: What Does a Leaf Estimate?

在 causal tree 的每個 leaf 裡，我們比較**同一個 leaf 內**的 treated 和 control 觀測值的均值差：

$$\hat{\tau}(\ell) = \bar{Y}^1_\ell - \bar{Y}^0_\ell$$

投影片的例子：

- Leaf 1（年輕、低教育程度工人）：treated 均值 $11,000，control 均值 $8,000，效果 = $3,000
- Leaf 2（年長、有豐富經驗工人）：treated 均值 $18,500，control 均值 $18,000，效果 = $500

對一個新工人，根據他的特徵 $x$，讓他落在某一個 leaf $\ell(x)$，然後：

$$\hat{\tau}(x) = \hat{\tau}(\ell(x))$$

也就是說，我們的預測是：跟你特徵相似的人，treatment effect 是多少。

---

## 【投影片】Causal Tree: Finding the Best Split

Causal tree 的切割標準和 prediction tree 不一樣：

- **Prediction tree**：找讓兩個 leaf 內的 **outcome** 最整齊的切割（RSS 最小）
- **Causal tree**：找讓兩個 leaf 的 **treatment effect** 差異最大的切割

具體做法：對候選切割 $(X^j, s)$，分成 $R_1$ 和 $R_2$ 兩個區域，分別估計：

$$\hat{\tau}(R_k) = \bar{Y}^1_{R_k} - \bar{Y}^0_{R_k}$$

然後計算：

$$\Delta(j, s) = |\hat{\tau}(R_1) - \hat{\tau}(R_2)|$$

選 $\Delta$ 最大的切割——也就是讓兩個葉節點的 treatment effect 差最多的那個。

---

## 【投影片】Causal Tree: A Small Split Example（小型分割例子）

投影片的表格：

| 切割 | $\hat{\tau}(R_1)$ | $\hat{\tau}(R_2)$ | $\Delta$ |
|---|---|---|---|
| Age < 30 | $3,000 | $600 | **$2,400** |
| Educ < 12 | $2,100 | $1,400 | $700 |
| Prior earn. < 5,000 | $2,600 | $1,000 | $1,600 |

在這個例子中，causal tree 選擇**age < 30** 作為第一個切割點。

注意：這不是因為年輕人和年長工人的**薪資水準**差最多，而是因為他們的**training 效果**差最多——年輕人效果 $3,000，年長工人只有 $600。

這是和 prediction tree 最本質的差異：causal tree 追的是效果的差異，而不是水準的差異。

---

## 【投影片】Causal Tree: Honest Estimation（誠實估計）

Causal tree 有一個非常重要的設計：**honesty（誠實性）**。

問題在哪裡？如果我們用**同一批資料**來決定切割點，又用來估計葉節點的效果，會產生 double-dipping（雙重使用資料）的問題：我們是在用同樣的資料「發現規律」又「報告規律的大小」，這會導致估計值過度樂觀。

解決方法是把一個隨機 subsample 分成兩半：

- **Splitting sample（切割樣本）**：用來學習要在哪裡切——找哪個分割讓 treatment effect 差最多
- **Estimation sample（估計樣本）**：用全新的資料計算每個 leaf 的 $\hat{\tau}(\ell)$

這樣，決定切割點的資料，和估計 leaf 效果的資料，是相互獨立的。這就是「honesty」——就像你不能既當原告又當法官，切割和估計要用不同的資料。

---

# 第四部分：Causal Forest

---

## 【投影片】From Random Forest to Causal Forest（類比：從隨機森林到因果森林）

這張投影片的目的是展示 random forest 和 causal forest 的結構完全類比：

| | Random Forest | Causal Forest |
|---|---|---|
| **建立什麼** | 很多棵 prediction trees | 很多棵 honest causal trees |
| **每棵樹估計** | $\hat{Y}_b(x)$ | $\hat{\tau}_b(x)$ |
| **平均** | $\hat{Y}(x) = \frac{1}{B}\sum_b \hat{Y}_b(x)$ | $\hat{\tau}(x) = \frac{1}{B}\sum_b \hat{\tau}_b(x)$ |

**核心句：Random forest 穩定化 outcome 的預測；causal forest 穩定化 CATE 的估計。**

---

## 【投影片】Causal Forest: Algorithm（演算法）

Causal forest 的演算法：

1. 從全部資料隨機抽取一個 subsample
2. 把這個 subsample 分成兩半：splitting sample 和 estimation sample
3. 用 splitting sample 學習切割點，讓 treatment effect 的差異最大化
4. 用 estimation sample 在每個 leaf 裡估計 $\hat{\tau}(\ell) = \bar{Y}^1_\ell - \bar{Y}^0_\ell$
5. 得到一棵 honest causal tree
6. 重複以上步驟，生長 $B$ 棵樹
7. 對一個有特徵 $x$ 的工人，平均所有樹的估計：

$$\hat{\tau}(x) = \frac{1}{B}\sum_{b=1}^{B}\hat{\tau}_b(x)$$

---

## 【投影片】Causal Forest: Illustration（多棵因果樹示意圖）

這張投影片用圖來呈現 causal forest 的流程。

對一個特徵為 $x$ 的工人（例如 age = 25，educ = 10）：

- 把他放進每一棵 honest causal tree
- 每棵樹把他分到某個 leaf，給出一個 CATE 估計：$2,800、$2,200、$3,100、$2,500……
- 把所有樹的估計平均，得到最終的 $\hat{\tau}(x)$

平均的功能是**消除每棵樹因為用不同 subsample 而引入的噪音**。如果每棵樹的估計誤差是獨立的，平均後的誤差會縮小；如果每棵樹的誤差是正相關的（因為它們都來自同一個資料），就不會完全消除，但還是能降低變異。

---

## 【投影片】Why Does Causal Forest Help?（為什麼有用？）

數學上，可以把 causal forest 的估計拆解成：

$$\hat{\tau}(x) = \tau(x) + \frac{1}{B}\sum_{b=1}^{B}\varepsilon_b(x)$$

$\tau(x)$ 是真實的 CATE；$\varepsilon_b(x)$ 是第 $b$ 棵樹的估計誤差。

如果各棵樹的誤差部分相互抵消，平均後的 $\hat{\tau}(x)$ 就會比任何一棵樹的估計更接近 $\tau(x)$。

**Random forest** 用這個方法穩定 outcome 的預測；**Causal forest** 用同樣的方法穩定 treatment effect 的估計。

---

## 【投影片】Causal Forest: Adaptive Matching Intuition（自適應配對直覺）

Causal forest 有一個很好的直覺解釋：它像是一種**自適應配對（adaptive matching）**。

對一個目標工人 $x$：
- 每棵樹把他分到某個 leaf
- 在這個 leaf 裡，其他工人（不管是 treated 還是 control）都被認為「跟他相似」
- 跨所有樹，常常和 $x$ 在同一個 leaf 的工人，就得到更大的權重 $a_i(x)$

最終估計近似於：

$$\hat{\tau}(x) \approx \sum_{i: D_i=1} a_i(x) Y_i - \sum_{i: D_i=0} a_i(x) Y_i$$

這就是一個加權版本的比較：用和 $x$ 相似的 treated 工人，減去和 $x$ 相似的 control 工人。

和傳統的 nearest-neighbor matching 不同的是：「相似」的定義不是事先固定的，而是**由 forest 從資料中學習**來的。

---

## 【投影片】Causal Forest: What Do We Get After Estimation?（估計之後得到什麼？）

跑完 causal forest 之後，我們可以得到幾個東西：

1. **Individual CATE estimates**：$\hat{\tau}(x_i)$，每個工人一個估計值。這是最精細的輸出。

2. **ATE**：把所有人的 CATE 平均起來：
   $$\hat{\alpha}_{\text{ATE}} \approx \frac{1}{N}\sum_{i=1}^{N}\hat{\tau}(x_i)$$

3. **ATT**：只對 treated 工人的 CATE 求平均：
   $$\hat{\alpha}_{\text{ATT}} \approx \frac{1}{N_1}\sum_{i: D_i=1}\hat{\tau}(x_i)$$

4. **Heterogeneity summaries**：CATE 的分佈、BLP、variable importance——用來描述和解釋 heterogeneity 的模式。

---

## 【投影片】Causal Forest: How to Interpret $\hat{\tau}(x_i)$（如何解讀 CATE 估計值？）

一個很重要的概念釐清：$\hat{\tau}(x_i)$ **不是**工人 $i$ 的「真實個人 treatment effect」。

為什麼？因為因果推論的根本問題（fundamental problem of causal inference）還在：我們永遠只能觀察到一個 potential outcome。工人 $i$ 如果接受了訓練，我們就沒有他不接受訓練的 outcome；如果他沒接受訓練，我們就沒有他接受訓練的 outcome。

正確的解讀是：

> $\hat{\tau}(x_i)$ 是跟工人 $i$ 特徵相似的人，平均 treatment effect 的估計值。

例子：如果 $\hat{\tau}(x_i) = \$2,500$，意思是「跟工人 $i$ 類似的工人，平均來說，參加訓練大約可以多賺 2,500 美元」。

**Causal forest 估計的是 CATE，不是每個人的真實反事實 outcome。**

---

## 【投影片】After Causal Forest: CATE Distribution（CATE 分佈）

估計 causal forest 之後，第一件事是看 CATE 的分佈。

把所有觀測值的 $\hat{\tau}(x_i)$ 畫成直方圖：

- 分佈的**中心**接近 ATE
- 分佈的**寬度**代表 heterogeneity 的大小
  - **寬**：不同工人的 treatment effect 差異很大，政策針對性（targeting）可能很有價值
  - **窄**：大多數工人的 treatment effect 相似，ATE 已經是一個很好的摘要

這張圖的**紅色虛線**標示 CATE 的均值，和 ATE 估計值很接近。

---

## 【投影片】After Causal Forest: Best Linear Projection (BLP)（最佳線性投影）

CATE 分佈告訴我們「有沒有 heterogeneity」；BLP 告訴我們「哪些特徵跟 CATE 的大小有關」。

BLP 的做法是把估計出來的 $\hat{\tau}(x_i)$ 當 dependent variable，然後迴歸在 covariates 上：

$$\hat{\tau}(x_i) = \beta_0 + \beta_1\,\text{age}_i + \beta_2\,\text{educ}_i + \beta_3\,\text{re74}_i + \cdots + \varepsilon_i$$

解讀：
- 如果 $\hat{\beta}_1 < 0$：年齡越大，估計的 training 效果越小
- 如果 $\hat{\beta}_3 < 0$：過去收入越高，估計的訓練效果越小

BLP 的優點是把複雜的 CATE 估計翻譯成大家熟悉的迴歸語言，方便解釋和溝通。

---

## 【投影片】After Causal Forest: Variable Importance（變數重要性）

Variable importance 回答的問題是：**哪些變數，forest 最常用來分割？**

計算方式：統計每個 covariate 在所有 $B$ 棵樹的所有切割中被使用的頻率。

解讀：

- 重要性高的變數，代表 forest 認為這個變數很有用來分辨高效果和低效果的工人
- 可以捕捉非線性效果和交叉項（interaction effects）
- 但**不告訴我們方向**——一個變數重要，不代表它越大效果越大或越小；方向的資訊要去看 BLP

在 LaLonde 的例子裡，age 和 re74（1974年的收入）是最重要的兩個變數，和直覺一致：年輕、過去收入低的工人，訓練效果最大。

---

# 第五部分：R Example

---

## 【投影片】R Example（章節標題）

好，現在進入 R code 的示範部分。這部分的目的是讓大家看到，從資料到 causal forest 輸出的完整流程。

---

## 【投影片】LaLonde Job Training Data（資料介紹）

我們用的是 LaLonde（1986）的 job training 資料。這是因果推論領域的一個經典資料集，Dehejia & Wahba（1999）後來也用它來示範 propensity score matching。

- **Treatment $D_i$**：有沒有參加職業訓練
- **Outcome $Y_i$**：1978 年的實際收入（訓練計畫在 1970 年代末期執行）
- **Covariates $X_i$**：年齡、教育年數、種族（black, hispanic）、婚姻狀態、是否沒有高中文憑（nodegr）、1974 和 1975 年的收入（re74, re75）

這個資料集的 treatment 是隨機分配的，所以 ATE 可以直接用 difference in means 來估計。這對 causal forest 很重要，因為隨機化提供了識別策略，causal forest 再告訴我們 heterogeneity。

---

## 【投影片】Data and Program（資料與程式）

這部分對應的程式碼檔案是 **`causal_forest.R`**。

不需要另外下載資料——`lalonde` 資料集內建在 `Matching` 套件裡，`library(Matching); data(lalonde)` 就可以載入。

需要安裝的套件：
- **`grf`**：核心套件，提供 `causal_forest()`、`variable_importance()`、`best_linear_projection()` 等函數
- **`Matching`**：提供 LaLonde 資料集
- **`ggplot2`**：畫 CATE 分佈圖和 variable importance 圖
- **`dplyr`**：做 subgroup 的分組摘要

完整流程：準備 $(Y, D, X)$ → 估計 causal forest → 計算 ATE/ATT → 取出 CATE 估計 → 畫圖、BLP、variable importance → 匯出資料

---

## 【投影片】Step 1: Package Installation and Data Loading

```r
install.packages(c("grf", "Matching", "ggplot2"))
library(grf)
library(Matching)
library(ggplot2)

data(lalonde)
summary(lalonde)
```

安裝和載入套件，然後載入 LaLonde 資料。`summary()` 讓我們先看資料的基本統計量。

注意：`grf` 代表「generalized random forests」，是實作 causal forest 最主流的 R 套件，由 Athey、Tibshirani、Wager 的研究團隊維護。

---

## 【投影片】Step 2: Data Preparation

```r
Y <- lalonde$re78
D <- lalonde$treat
X <- lalonde[, c("age", "educ", "black", "hisp",
                 "married", "nodegr", "re74", "re75")]
X <- as.matrix(X)
```

把 outcome、treatment 和 covariates 分別抽出來。`grf` 要求 $X$ 是 matrix 格式，所以要用 `as.matrix()` 轉換。

注意：`grf` 的函數把 treatment 叫做 `W`，這是它的命名習慣，但概念上就是我們課程記號裡的 $D_i$。

---

## 【投影片】Step 3: Causal Forest Estimation

```r
set.seed(2026)

cf <- causal_forest(
  X = X,
  Y = Y,
  W = D,
  num.trees = 2000
)
```

`set.seed(2026)` 是為了讓結果可以重現（reproducible）。因為 causal forest 用的是隨機抽樣，每次跑的結果會稍有不同；設定 seed 之後，每次跑出來的結果都一樣。

`num.trees = 2000` 代表生長 2000 棵樹。樹越多，估計越穩定，但計算時間也越長。2000 是一個不錯的起點；在正式研究中通常用 4000 棵或更多。

---

## 【投影片】Step 4: Estimate ATE and ATT

```r
ate <- average_treatment_effect(cf, target.sample = "all")
att <- average_treatment_effect(cf, target.sample = "treated")

ate
att
```

`average_treatment_effect()` 從 causal forest 的輸出計算 ATE 和 ATT，並提供標準誤。

- `target.sample = "all"`：對全部樣本求平均，估計 ATE
- `target.sample = "treated"`：只對 treated 樣本求平均，估計 ATT

---

## 【投影片】What ATE and ATT Mean Here

這頁解釋估計出來的數字意義。

在這個例子中，ATE 大約是 1,583 美元，代表職業訓練計畫平均讓工人一年多賺約 1,583 美元。ATT 約 1,792 美元，代表在實際接受訓練的工人中，訓練效果平均更高一點。

注意：即使 ATE 和 ATT 都是正的，並不意味著對每個人都有同樣的效果。接下來的 CATE 分析才能告訴我們誰受益更多。

---

## 【投影片】Step 5: Get CATE Estimates

```r
tau_hat <- predict(cf)$predictions

summary(tau_hat)
```

`predict(cf)$predictions` 取出每個觀測值的 CATE 估計值。

`tau_hat` 是一個長度為 $N$ 的向量，`tau_hat[i]` 是工人 $i$ 的 CATE 估計值。

`summary()` 告訴我們 CATE 分佈的基本資訊：最小值、中位數、均值、最大值。在這個例子中，CATE 的範圍大約從 -$110 到 $4,043，標準差約 $1,022，顯示確實存在相當程度的 heterogeneity。

---

## 【投影片】Step 6: Plot CATE Distribution（code）

```r
cate_df <- data.frame(tau_hat = tau_hat)

ggplot(cate_df, aes(x = tau_hat)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(tau_hat),
             color = "red", linetype = "dashed") +
  labs(x = "Estimated treatment effect",
       y = "Count")
```

把 CATE 估計值存成 data frame，然後用 `ggplot2` 畫直方圖。紅色虛線標示 CATE 的均值，大約等於 ATE 估計值。

---

## 【投影片】Step 6: CATE Distribution（圖）

這就是跑出來的圖。

可以看到 CATE 的分佈呈右偏，大多數工人的 treatment effect 在 0 到 3,000 美元之間，少數工人的估計效果很高（接近 4,000 美元）。

紅色虛線（均值）接近 ATE 的估計值，和預期一致。

分佈的寬度告訴我們：heterogeneity 確實存在，治療計畫對不同工人的幫助差異很大。這意味著如果資源有限，針對性的政策設計可能比普遍式的計畫更有效率。

---

## 【投影片】Step 7: Summarize Heterogeneity（code）

```r
# Best linear projection
blp <- best_linear_projection(cf, X)
blp

# Variable importance
vi <- variable_importance(cf)
names(vi) <- colnames(X)
sort(vi, decreasing = TRUE)
```

`best_linear_projection()` 把 CATE 估計值迴歸在 covariates 上，提供線性摘要。

`variable_importance()` 計算每個 covariate 在所有樹的切割中被使用的頻率，回傳一個重要性分數向量。`sort(..., decreasing = TRUE)` 按重要性從高到低排列。

---

## 【投影片】Step 7: Variable Importance（圖）

這是 variable importance 的橫條圖。

從圖中可以看到：

1. **Age（年齡）** 是最重要的變數，重要性接近 0.32
2. **re74（1974年收入）** 排第二，約 0.21
3. **educ（教育年數）** 排第三，約 0.17
4. **re75（1975年收入）** 排第四

也就是說，forest 在分割的時候，最常用年齡和過去的收入水準來區分高效果和低效果的工人。這和直覺一致：年輕、過去收入低的工人，訓練效果往往最大。

---

## 【投影片】Step 8: Export for Stata Post-Analysis

```r
results <- data.frame(
  id      = 1:nrow(X),
  tau_hat = tau_hat,
  age     = lalonde$age,
  educ    = lalonde$educ,
  D       = lalonde$treat
)

write.csv(results, "cate_results.csv", row.names = FALSE)
```

把 CATE 估計值和原始資料欄位合在一起，匯出成 CSV 檔。

這讓你可以把 R 的估計結果帶回 Stata，做進一步的分析：例如用 Stata 的表格套件報告各 subgroup 的平均 CATE，或是做 heterogeneity 的驗證測試。

在研究流程中，R 負責計算 causal forest，Stata 負責最後的整理和報告，是很常見的搭配。

---

## 【投影片】Recommended Resources

最後推薦幾個參考資料：

1. **Wager & Athey (2018)**，《Estimation and Inference of Heterogeneous Treatment Effects using Random Forests》，*JASA* — Causal forest 的原始論文，推導了漸近常態性和有效標準誤

2. **Athey, Tibshirani & Wager (2019)**，《Generalized Random Forests》，*Annals of Statistics* — 把 causal forest 推廣到更一般的 framework

3. **`grf` 套件文件**：[grf-labs.github.io/grf](https://grf-labs.github.io/grf/)  — 最實用的參考，包含函數說明、vignettes 和範例

4. **Athey & Imbens (2019)**，《Machine Learning Methods That Economists Should Know About》，*Annual Review of Economics* — 非技術性的概述，很好讀，適合作為入門閱讀

---

# 課程總結

今天這一講從以下幾個層次建立了 causal forest 的理解：

1. **動機**：ATE 不夠，我們想要 CATE $\tau(x)$，知道誰受益更多

2. **工具箱**：Decision tree → Random forest → Causal tree → Causal forest，每一步都有明確的類比

3. **Causal forest 的核心**：很多棵 honest causal trees 的平均，每棵樹用 subsample 訓練、用 honesty 保證估計的有效性

4. **重要概念**：Causal forest **不是**識別策略；它是一個彈性的估計量，需要搭配 RCT、DID 或 RDD 才有因果詮釋

5. **輸出的解讀**：CATE 分佈（有沒有 heterogeneity）、BLP（哪個方向有關聯）、variable importance（哪個變數重要）

6. **實作**：用 `grf` 套件，資料是 LaLonde job training 資料，完整流程見 `causal_forest.R`
