% rewrites the provided data into usable files
% (which can be used in R)
%   1. EQ count (using declustered catalog)
%   2. SSN

clear all;


%% Get EQ count

NEICdecluster = load('data/NEIC-decluster');

% Daily bins

[A,a,c] = unique(NEICdecluster(:,1:3),'rows');
n = hist(c,length(a))';
EQs_day = datevec(datenum([1900 01 01]):datenum([2012 12 31]));
EQs_dailyCnt = [A n];

% Takes some time... (1-2min)
for i=1:length(EQs_day)
    ind = find(ismember(EQs_dailyCnt(:,1:3),EQs_day(i,1:3),'rows')>0);
    if ind > 0
        EQs_day(i,4) = EQs_dailyCnt(ind,4);
    end
end

% Monthly bins

j = 1;
for year=1900:2012
    for month=1:12
        YM = [year month];
        cnt = nnz(ismember(NEICdecluster(:,1:2),YM,'rows'));
        EQs_mon(j,:) = [YM cnt];
        j = j + 1;
    end
end

% Yearly bins (builds upon monthly bins)

j = 1;
for year=1900:2012
    subs = find(EQs_mon(:,1)==year);
    cnt = sum(EQs_mon(subs,3));
    EQs_year(j,:) = [year cnt];
    j = j + 1;
end



%% Rewrite (monthly) SSN

fid = fopen('data/SSN');
c = textscan(fid, '%f %f %f %f %f');
fclose(fid);

SSN_mon = cell2mat(c(:,[1 2 4]));

Discretize = 5; % set the discretizing interval

SSN_mon_discr = [SSN_mon(:,1:2) round(SSN_mon(:,3)/Discretize)*Discretize];

% Yearly bins
j = 1;
for year=min(SSN_mon(:,1)):max(SSN_mon(:,1))
        subs = find(SSN_mon(:,1)==year);
        cnt = mean(SSN_mon(subs,3));
        SSN_year(j,:) = [year cnt];
        j = j + 1;
end
SSN_year_discr = [SSN_year(:,1) round(SSN_year(:,2)/Discretize)*Discretize];



%% PLOT

figure
plot(EQs_mon(:,3),SSN_mon_discr(:,3),'.')
figure
plot(EQs_year(:,2),SSN_year_discr(:,2),'.')
xlabel('EQ count per year')
ylabel('average sunspot number per year')



% %% OUT
% 
% fout = fopen('data/EQs-day','w');
% for i = 1:length(EQs_day)
%     fprintf(fout,'%4d %2d %2d %1d\n',EQs_day(i,1:4));
% end
% fclose(fout);
%
% fout = fopen('data/EQs-mon','w');
% for i = 1:length(EQs_mon)
%     fprintf(fout,'%4d %2d %2d\n',EQs_mon(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/EQs-year','w');
% for i = 1:length(EQs_year)
%     fprintf(fout,'%4d %2d\n',EQs_year(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/SSN-mon','w');
% for i = 1:length(SSN_mon)
%     fprintf(fout,'%4d %2d %3.1f\n',SSN_mon(i,:));
% end
% fclose(fout);
% fout = fopen(sprintf('data/SSN-mon-%d',BIN),'w');
% for i = 1:length(SSN_monbin)
%     fprintf(fout,'%4d %2d %3d\n',SSN_mon_discr(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/SSN-year','w');
% for i = 1:length(SSN_year)
%     fprintf(fout,'%4d %3.1f\n',SSN_year(i,:));
% end
% fclose(fout);
% fout = fopen(sprintf('data/SSN-year-%d',BIN),'w');
% for i = 1:length(SSN_yearbin)
%     fprintf(fout,'%4d %3d\n',SSN_year_discr(i,:));
% end
% fclose(fout);
