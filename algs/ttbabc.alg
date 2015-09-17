\begin{algorithmic}

\State
\Function{ttbabclearn}{$X_{learn},~ y_{learn},~\epsilon,~ \rho,~ \eta$}

\State counter $\gets 0$ \Comment initialize counter

\While{counter$<\eta$} \Comment $\eta$ simulations

\State sample $\rho$ of $\mathcal{D_{\rho}}=\{X_{\rho}, y_{\rho}\}$ 
\Comment test set

\State sample $p_i \sim \mathcal{B}(\alpha+k_{i},\beta-k_{i}+n_{trials})$
\Comment{beta-binomial posterior}

\State proposal-tree: $\mathcal{T}:p(C_{i}= pos_{i}) \propto p_i$ 
\Comment sampling without replacement
\State predict $y_{prop}=\mathcal{T}(X_\rho)$
\Comment y proposal
\State calculate $\delta=\sum_{i=1}^\rho |y_{\rho i}-y_{pred i}|$
\Comment calculate distance
\If{$\delta < \epsilon$}
\Comment accept if smaller than $\epsilon$
\State $k_{i} \gets k_{i}+1$
\Comment update count if successful 
\State $n_{trials} \gets n_{trials}+1$
\Comment update trials
\State counter $\gets$ counter+1
\Comment increment counter
\EndIf
\EndWhile  
\State \Return $k$, $n_{trials}$
\Comment return counts
\EndFunction

\State
\Function{ttbabcpredict}{$k, ~n_{trials}, ~X_{predict},~\eta_{aggregate}$}

\For{i in 1:$\eta_{aggregate}$}
\Comment generate $\eta_{aggregate}$ predictions
 \State sample $p_i \sim \mathcal{B}(\alpha+k_{i},\beta-k_{i}+n_{trials})$
\Comment{sample from posterior}
\State proposal-tree: $\mathcal{T}:p(C_{i}= pos_{i}) \propto p_i$ 
\Comment create proposal tree
\State $y_{pred} \gets \mathcal{T}(X_{predict})$
\Comment generate prediction
\EndFor
 \State \Return $y_{predict}$
\Comment return aggregated prediction
\EndFunction
\State
\end{algorithmic}
