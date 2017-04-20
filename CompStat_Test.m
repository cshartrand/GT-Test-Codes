%Question 4 Training Set
T = readtable('training.csv');
M = table2array(T);
p=27;
numit = 100;
step = 1;
x = M(:,2:28);
y = M(:,1);
n=198;
yc = zeros(198,3);
for i=1:198
    if y(i,1)==1
        yc(i,1)=1;
    elseif y(i,1)==2
        yc(i,2)=1;
    else 
        yc(i,3)=1;
    end
end
y1 = yc(:,1);
y2 = yc(:,2);
y3 = yc(:,3);
%Run logistic regression for 'o' 's' 'h' 'd' coded as 0,1,2,3 respectively.
betap1=zeros(p+1,1);
betap2=zeros(p+1,1);
betap3=zeros(p+1,1);
betas = [betap1; betap2; betap3];
for i=1:numit
    pi1 =exp(x*betap1(1:p)+betap1(p+1))./(1+(exp(sum(x*betap1(1:p)+betap1(p+1)))+exp(sum(x*betap2(1:p)+betap2(p+1)))+exp(sum(x*betap3(1:p)+betap3(p+1)))));
    pi2 =exp(x*betap2(1:p)+betap2(p+1))./(1+(exp(sum(x*betap1(1:p)+betap1(p+1)))+exp(sum(x*betap2(1:p)+betap2(p+1)))+exp(sum(x*betap3(1:p)+betap3(p+1)))));
    pi3 =exp(x*betap3(1:p)+betap3(p+1))./(1+(exp(sum(x*betap1(1:p)+betap1(p+1)))+exp(sum(x*betap2(1:p)+betap2(p+1)))+exp(sum(x*betap3(1:p)+betap3(p+1)))));
    pi = [pi1 pi2 pi3];
    
    gradient1=[x ones(n,1)]'*(y1-pi1);
    gradient2=[x ones(n,1)]'*(y2-pi2);
    gradient3=[x ones(n,1)]'*(y3-pi3);
    grads = [gradient1 ;gradient2 ;gradient3];
    
    H11 = -pi1'*(ones(n,1)-pi1)*[x ones(n,1)]'*[x ones(n,1)];
    H22 = -pi2'*(ones(n,1)-pi2)*[x ones(n,1)]'*[x ones(n,1)];
    H33 = -pi3'*(ones(n,1)-pi3)*[x ones(n,1)]'*[x ones(n,1)];
    H12off=-[x ones(n,1)]'*[x ones(n,1)]*(pi1'*pi2);
    H13off=-[x ones(n,1)]'*[x ones(n,1)]*(pi1'*pi3);
    H23off=-[x ones(n,1)]'*[x ones(n,1)]*(pi2'*pi3);
    H21off =-[x ones(n,1)]'*[x ones(n,1)]*(pi2'*pi1);
    H31off =-[x ones(n,1)]'*[x ones(n,1)]*(pi3'*pi1);
    H32off =-[x ones(n,1)]'*[x ones(n,1)]*(pi3'*pi2);
    H = [H11 H12off H13off;H21off H22 H23off; H32off H32off H33];

    betas = betas - step*inv(H)*grads;
end
betap1 = betas(1:28,1);
betap2 = betas(29:56,1);
betap3 = betas(57:84,1);

%Now for the testing dataset
T2 = readtable('testing.csv');
M2 = table2array(T2);
Mx = [M2(:,2:28) ones(325,1)];
My = M2(:,1);
p1=[];
for i =1:325 %Probability of 's'
    p1(1,i) = exp(Mx(i,:)*betap1)/(1+exp(Mx(i,:)*betap1)+exp(Mx(i,:)*betap2)+exp(Mx(i,:)*betap3));
end
p1 = p1';
p2=[];
for i =1:325 %Probability of 'h'
    p2(1,i) = exp(Mx(i,:)*betap2)/(1+exp(Mx(i,:)*betap1)+exp(Mx(i,:)*betap2)+exp(Mx(i,:)*betap3));
end
p2 = p2';
p3=[];
for i =1:325 %Probability of 'd'
    p3(1,i) = exp(Mx(i,:)*betap3)/(1+exp(Mx(i,:)*betap1)+exp(Mx(i,:)*betap2)+exp(Mx(i,:)*betap3));
end
p3 = p3';
temp = p1+p2+p3;
p4 = ones(325,1) - temp; %Calculated probability of 'o;
pt = [p1 p2 p3 p4];
pred =[];
for i=1:325
    [num idx] =max(pt(i,:));
    pred = [pred idx];
end
pred = pred';
for i =1:325
    if pred(i,1)==4;
        pred(i,1)=0;
    end
end
check = My - pred;
count=0;
for i=1:325
    if check(i,1)~=0
        count = count+1;
    end
end
classerror = count/325; %Classification Error Percentage