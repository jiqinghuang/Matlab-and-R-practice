function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
% for i = 1:m
%    step = y(i)*log( sigmoid( X(i,:)*theta ) ) + ( 1-y(i) )* log( 1 - sigmoid( X(i,:)*theta ) );
%    J = J - 1/m * step;
% end
% J = J + lambda/(2*m) * ( sum(theta.^2) - theta(1)^2 );   % note can not regularize theta_{0}

J = (-y' * log( sigmoid( X*theta ) ) - ( 1-y' )* log( 1 - sigmoid( X*theta ) ) )/m + lambda/(2*m) * ( sum(theta.^2) - theta(1)^2 );

grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta
% grad(1) = 1/m * ( X(:,1)' * (sigmoid( X *theta ) - y) );
% grad(2) = 1/m * ( X(:,2)' * (sigmoid( X *theta ) - y) ) + lambda/m * theta(2);
% grad(3) = 1/m * ( X(:,3)' * (sigmoid( X *theta ) - y) ) + lambda/m * theta(3);
t = ones(size(theta));
t(1) = 0;
grad = 1/m * X' * (sigmoid(X * theta) - y) + lambda * (theta .* t)/ m;



% =============================================================

end
