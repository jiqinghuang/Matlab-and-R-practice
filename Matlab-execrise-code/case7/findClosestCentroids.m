function idx = findClosestCentroids(X, centroids)
%FINDCLOSESTCENTROIDS computes the centroid memberships for every example
%   idx = FINDCLOSESTCENTROIDS (X, centroids) returns the closest centroids
%   in idx for a dataset X where each row is a single example. idx = m x 1 
%   vector of centroid assignments (i.e. each entry in range [1..K])
%

% Set K
K = size(centroids, 1);

% You need to return the following variables correctly.
idx = zeros(size(X,1), 1);

% ====================== YOUR CODE HERE ======================
% Instructions: Go over every example, find its closest centroid, and store
%               the index inside idx at the appropriate location.
%               Concretely, idx(i) should contain the index of the centroid
%               closest to example i. Hence, it should be a value in the 
%               range 1..K
%
% Note: You can use a for-loop over the examples to compute this.
%


% for i = 1:length(idx);
%    target = X(i,:);
%    index = zeros(K, 1);
    % can use repmat if want to stack vetor as matrix.
%    for j = 1:K;
%        distance = centroids(j,:) - target ;
%        index(j) = distance * distance'; 
%    end
%    [a, b] = min(index);
%    idx(i) = b;
% end


% use one for loop rather than double for loop.
for i = 1:length(idx);
    target = repmat(X(i, :), K, 1);
    step1 = centroids - target;
    step2 = step1.^2;
    step3 = sum(step2, 2);
    [a, b] = min(step3);
    idx(i) = b;
end




% =============================================================

end

