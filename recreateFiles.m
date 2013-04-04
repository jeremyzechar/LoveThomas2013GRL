% rewrites the provided data into usable files
% (which can be used in R)
%   1. EQ count (using declustered catalog)
%   2. SSN

clear all;


%% Get monthly/yearly EQ count

NEICdecluster = load('data/NEIC-decluster');
[A,a,c] = unique(NEICdecluster(:,1:2),'rows');
n = hist(c,length(a))';

EQs_mon = [A n];

j = 1;
for year=min(NEICdecluster(:,1)):max(NEICdecluster(:,1))
    for month=1:12
        YM = [year month];
        cnt = nnz(ismember(NEICdecluster(:,1:2),YM,'rows'));
        EQs_mon(j,:) = [YM cnt];
        j = j + 1;
    end
end

% do per year
j = 1;
for year=min(NEICdecluster(:,1)):max(NEICdecluster(:,1))
        subs = find(EQs_mon(:,1)==year);
        cnt = sum(EQs_mon(subs,3));
        EQs_year(j,:) = [year cnt];
        j = j + 1;
end



%% Rewrite SSN

fid = fopen('data/SSN');
c = textscan(fid, '%f %f %f %f %f');
fclose(fid);

SSN_mon = cell2mat(c(:,[1 2 4]));

BIN = 5; % set the bin interval

SSN_monbin = [SSN_mon(:,1:2) round(SSN_mon(:,3)/BIN)*BIN];

% do per year
j = 1;
for year=min(SSN_mon(:,1)):max(SSN_mon(:,1))
        subs = find(SSN_mon(:,1)==year);
        cnt = mean(SSN_mon(subs,3));
        SSN_year(j,:) = [year cnt];
        j = j + 1;
end
SSN_yearbin = [SSN_year(:,1) round(SSN_year(:,2)/BIN)*BIN];



%% PLOT

figure
plot(EQs_mon(:,3),SSN_monbin(:,3),'.')
figure
plot(EQs_year(:,2),SSN_yearbin(:,2),'.')
xlabel('EQ count per year')
ylabel('average sunspot number per year')



% %% OUT
% 
% fout = fopen('data/EQs-mon.dat','w');
% for i = 1:length(EQs_mon)
%     fprintf(fout,'%4d %2d %2d\n',EQs_mon(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/EQs-year.dat','w');
% for i = 1:length(EQs_year)
%     fprintf(fout,'%4d %2d\n',EQs_year(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/SSN-mon.dat','w');
% for i = 1:length(SSN_mon)
%     fprintf(fout,'%4d %2d %3.1f\n',SSN_mon(i,:));
% end
% fclose(fout);
% fout = fopen(sprintf('data/SSN-mon-bin-%d.dat',BIN),'w');
% for i = 1:length(SSN_monbin)
%     fprintf(fout,'%4d %2d %3d\n',SSN_monbin(i,:));
% end
% fclose(fout);
% 
% fout = fopen('data/SSN-year.dat','w');
% for i = 1:length(SSN_year)
%     fprintf(fout,'%4d %3.1f\n',SSN_year(i,:));
% end
% fclose(fout);
% fout = fopen(sprintf('data/SSN-year-bin-%d.dat',BIN),'w');
% for i = 1:length(SSN_yearbin)
%     fprintf(fout,'%4d %3d\n',SSN_yearbin(i,:));
% end
% fclose(fout);
