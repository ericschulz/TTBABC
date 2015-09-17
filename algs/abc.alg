\begin{algorithmic}
\Require Prior $\pi(\theta)$; model $\mathcal{M}$; data $\mathcal{D}=(\mathbf{X}, \mathbf{y})$; tolerance $\epsilon$
\For{$j=1,2,\dots$}\\
 \hspace{5mm}\textbf{Sample} $\theta^*\sim \pi(\theta)$\\
\hspace{5mm}\textbf{Simulate} $\mathbf{y^*}=\mathcal{M}_{\theta^*}(\mathbf{X})$\\
\hspace{5mm}\textbf{Calculate} $\delta=\rho \big(\mathcal{S}(\mathbf{y}),\mathcal{S}(\mathbf{y^*})\big)$
\If {$ \delta\leq \epsilon$}
    \State accept $\theta^*$
\Else
            \State reject $\theta^*$
\EndIf
\EndFor
\textbf{end for}
\end{algorithmic}


