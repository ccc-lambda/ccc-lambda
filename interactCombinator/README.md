

看到一個很有趣的新程式語言 bend

基本上有 python 的語法

但卻擁有完全不同的靈魂

繼承了 lambda calculus 與 interaction combinators

然後卻能很容易的接上 GPU/Cuda

甚至在未來可能成為神經網路 IC 的基礎

* [Interaction Combinators](https://core.ac.uk/download/pdf/81113716.pdf), information and computation 137, 69~101 (1997)

* https://en.wikipedia.org/wiki/Interaction_nets
* https://higherorderco.com/
* https://www.threads.net/@live_as_a_vagabond/post/DC3DmdMzUD9

    號外！ 在 GPU 上運行高級語言 出現了！

    經過CS 10年的不懈努力，上週末，有個叫 Bend 的編程語言在開源社區引發了熱烈的討論，GitHub 的 Star 量已經超過了 8500。
    
    github.com/Highe…
    
    作為一種大規模並行的高級編程語言，它仍處於研究階段，但提出的思路已經讓人們感到非常驚訝。使用 Bend，你可以為多核 CPU/
    GPU 編寫並行代碼，而無需成為具有 10 年經驗的 C/CUDA 專家，感覺就像 Python 一樣。
    
    有別於 CUDA、Metal 等低級別的替代方案，Bend 具有 Python、Haskell 等表達性語言的功能，包括快速對象分配、完全閉包支持的高階函數、無限制的recursion，甚至 continuation。Bend 運行在大規模並行硬件上，具有基於核心數量的近線性加速。
    Bend 由 HVM2 運行時提供支持。
    
    該項目的主要貢獻者 Victor Taelin 來自巴西，他在 X 平台上分享了 Bend 的主要特性和開發思路


* https://docs.google.com/viewer?url=https://raw.githubusercontent.com/HigherOrderCO/HVM/main/paper/HVM2.pdf
* https://www.semanticscholar.org/paper/Interaction-Combinators-Lafont/6cfe09aa6e5da6ce98077b7a048cb1badd78cc76

