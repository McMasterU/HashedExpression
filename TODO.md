* MRI Optimization Sense Problem

\begin{align*}
\min \limits_{S_i, \rho} &\sum \limits_{i} || \pi_{PE} (FT(S_iP_i))-m_i||^2 \\
&+ \lambda_1 (||\delta_x \rho ||_{L1} + || \delta_y \rho ||_{L1}) \\
&+ \lambda_2 (||\delta_x S_i ||_{L2} + || \delta_y S_i ||_{L2}) \\
\end{align*}
Would replace $S_i $ with $B_i$ 
\begin{align*}
\lambda_3 ||\nabla \cdot B_i ||^2 + \lambda_4 || \nabla \times B_i ||^2
\end{align*}

* TODOs
** TODO encode above objective in Hashed Expression
** TODO solve with LBFGS
** TODO finish integrating ipopt to new HashedToC (Curtis)
** TODO need constraint types (box, maybe conics?)
** TODO add initial values for optimziation problem
** TODO add histogram to maximize information in rho/S problem
** TODO add interior-point search directions to lBFGS
** TODO add box and cone-box constraints 
** TODO add latex output
