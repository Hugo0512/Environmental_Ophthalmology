clc
clear all
warning off
variablenumber=16;%%%%%%%%%%%%%%%%%%%%%%%
totaldata={};
sigma=5;
order=2;%阶数%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%运行时修改
%先填充空缺值
%之后分训练数据和验证数据
indepenedent_variable_number=15;
dependent_variable_number=1;%%%%%%%%%%%%%%%%%%%%%%%%%%%%运行时修改

parpool('local', 10);
raw=[];
climate=load('climate.txt');
air=load('air.txt');
diseases=load('diseases.txt');
climate=flipud(climate);
diseases=flipud(diseases);
raw=[air climate diseases];
                
               
                for colindex=1:size(raw,2)
                    raw(:,colindex)=(raw(:,colindex)-min(raw(:,colindex)))/(max(raw(:,colindex))-min(raw(:,colindex)));
                end
                %分训练和验证数据，训练并且测试
                
                
      for dependent_index=16:26   
          
          
                traindata=raw(1:round(size(raw,1)*0.8),[1:15,dependent_index]);
                testdata=raw(round(size(raw,1)*0.8)+1:end,[1:15,dependent_index]);
                
                lastresults=[];
                lastfitness=[];
                
                parfor index777=1:10
    index777
    
% for index000=9%6%:11
   
    historybestindividual=[];
    historybestfitness=Inf;
sparseratio=0.4;    
N=100;%种群规模
pop=[];%种群
for index=1:N
    for index1=1:variablenumber
        for index2=1:variablenumber
            if rand()<1-sparseratio-30/variablenumber^2
                pop(index,variablenumber*(index1-1)+index2)=0;
            else
                pop(index,variablenumber*(index1-1)+index2)=rand()*2-1;
            end
            pop(index,(indepenedent_variable_number*(indepenedent_variable_number+dependent_variable_number)+1):(indepenedent_variable_number*(indepenedent_variable_number+dependent_variable_number)+indepenedent_variable_number))=0;%患者数量对其他因素的影响忽略
%            pop(index,variablenumber*(index1-1)+index2)=rand()*2-1;
        end
    end
    
    
    
end

F=0.8;%向量因子
maxiterations=500;%进化代数
CR=0.2;
for index1=1:maxiterations
    index1
    for index2=1:N
       middlearray=pop(index2,:);
       selected=randperm(N);
        mutationarray=pop(selected(3),:)+F*(pop(selected(1),:)-pop(selected(2),:));
        crossnum=0;
        for index3=1:variablenumber^2
            label=rand();
            if(label<CR)
                middlearray(index3)=mutationarray(index3);
                croassnum=crossnum+1;
            end
        end
           if crossnum==0
%                exchangepoint=randi(1,1,[1 variablenumber^2]);
                 exchangepoint=randi([1 variablenumber^2],[1 1]);
               middlearray(exchangepoint)=mutationarray(exchangepoint);          
           end
%         fitness1=(middlearray(1)-7)*(middlearray(1)-7)+(middlearray(2)+6)*(middlearray(2)+6)+(middlearray(3)-9)*(middlearray(3)-9);
%         fitness2=(pop(index2,1)-7)*(pop(index2,1)-7)+(pop(index2,2)+6)*(pop(index2,2)+6)+(pop(index2,3)-9)*(pop(index2,3)-9);
%         if fitness1<fitness2
%             pop(index2,:)=middlearray;
%         end
   for index9=1:numel(middlearray)
           if middlearray(index9)>1
               middlearray(index9)=1;
           end
            if middlearray(index9)<-1
               middlearray(index9)=-1;
            end
   end
           middlearray((indepenedent_variable_number*(indepenedent_variable_number+dependent_variable_number)+1):(indepenedent_variable_number*(indepenedent_variable_number+dependent_variable_number)+indepenedent_variable_number))=0;%患者数量对其他因素的影响忽略
    %减少非0数值的个数
       nonzero=find(middlearray~=0);
       nonzeronumber=numel(nonzero);
       if nonzeronumber>variablenumber^2*(1-sparseratio)-variablenumber
           needzero=round(variablenumber^2*(1-sparseratio)-variablenumber);
%            position=randi(1,needzero,[1,variablenumber^2]);
           position=randi([1,variablenumber^2],[1 needzero]);
              %选择几个变成0
              middlearray(position)=0;
       end
    %最后一行修正为0
    
    %最后一行修正为0
%     for index=6:variablenumber
%      middlearray((index-1)*variablenumber+1:(index-1)*variablenumber+5)=0;
%     end
%贪婪选择（middlearray和pop(index2,:)）
%评价两个人塞体的适应度
%首先计算每个序列的数值,染色体恢复成矩阵
middlearraymatrix=[];
        for index4=1:variablenumber
            for index5=1:variablenumber
                middlearraymatrix(index4,index5)=middlearray((index4-1)+index5);
            end
        end
%计算每个序列的值
computationresult={};

  tempsequence=traindata;%%%%%%%%%%%%%%%%%%%%%%%%%%%
  sequence=[];
  sequence(1:order,:)=tempsequence(1:order,:);%存储模型产生的序列值
  for index7=order+1:size(tempsequence,1)
    for index6=1:variablenumber
             temp=0;
            for index789=1:order
                temp=temp+(sequence(index7-index789,:).^index789)*middlearraymatrix(:,index6);
                %sequence(index7,index6)=sequence(index7-1,:)*middlearraymatrix(:,index6);
            end
         sequence(index7,index6)=1/(1+exp(-sigma*temp));
        end
  end
  computationresult=sequence;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%计算误差
fitness1=0;

     sequence=computationresult;
     tempsequence=traindata;
        fitness1=fitness1+sum(sum((tempsequence(2:end,:)-sequence(2:end,:)).^2))/(variablenumber*(size(tempsequence,1)-1));

%首先计算每个序列的数值,染色体恢复成矩阵
middlearraymatrix=[];
anothervector=pop(index2,:);
        for index4=1:variablenumber
            for index5=1:variablenumber
                middlearraymatrix(index4,index5)=anothervector((index4-1)+index5);
            end
        end
%计算每个序列的值
computationresult1={};

  tempsequence=traindata;
    sequence1=[];
  sequence1(1:order,:)=tempsequence(1:order,:);%存储模型产生的序列值
  for index7=order+1:size(tempsequence,1)
    for index6=1:variablenumber
             temp=0;
            for index789=1:order
                temp=temp+(sequence1(index7-index789,:).^index789)*middlearraymatrix(:,index6);
                %sequence(index7,index6)=sequence(index7-1,:)*middlearraymatrix(:,index6);
            end
         sequence1(index7,index6)=1/(1+exp(-sigma*temp));
        end
  end
    computationresult1=sequence1;

%计算误差
fitness2=0;

    sequence1=computationresult1;
    tempsequence=traindata;
    fitness2=fitness2+sum(sum((tempsequence(2:end,:)-sequence1(2:end,:)).^2))/(variablenumber*(size(tempsequence,1)-1));



    if fitness1<fitness2
            pop(index2,:)=middlearray;
    end 
    if min(fitness1,fitness2)<historybestfitness
        historybestfitness=min(fitness1,fitness2);
        if fitness1<fitness2
          historybestindividual=middlearray;
        else
            historybestindividual=pop(index2,:);
        end
    end
    %修正取值范围
 
       end
end
%找出最好的一个
result=[];
fitness=zeros(1,N);
for index=1:N
    %找出每一个个体，计算适应度
    tempvector=pop(index,:);
     for index4=1:variablenumber
            for index5=1:variablenumber
                middlearraymatrix(index4,index5)=tempvector((index4-1)+index5);
            end
     end
     
          tempsequence=testdata;
          sequence1=[];
         sequence1(1:order,:)=tempsequence(1:order,:);%存储模型产生的序列值
         for index7=order+1:size(tempsequence,1)
            for index6=1:variablenumber
                 temp=0;
                for index789=1:order
                    temp=temp+(sequence1(index7-index789,:).^index789)*middlearraymatrix(:,index6);
                    %sequence(index7,index6)=sequence(index7-1,:)*middlearraymatrix(:,index6);
                end
                   sequence1(index7,index6)=1/(1+exp(-sigma*temp));
            end
        end
       computationresult=sequence1;
 
   
         tempsequence=testdata;
         sequence1=computationresult;
         fitness(index)=fitness(index)+sum(sum((tempsequence-sequence1).^2))/(variablenumber*(size(tempsequence,1)-1));
     
end
        if min(fitness)<historybestfitness
            number=find(fitness==min(fitness));
            historybestindividual=pop(number,:);
            historybestfitness=min(fitness);
        end
% end%与index000匹配
lastresult(index777,:)=historybestindividual;
lastfitness(index777)=historybestfitness;
%lastresults(index777,:)=historybestindividual;
lastfitness(index777)= historybestfitness;
lastresults=[lastresults;historybestindividual];
%lastfitness=[lastfitness historybestfitness];

end%对应index777
         save(['lastresult_',num2str(order),'_',num2str(dependent_index),'.mat'],'lastresults');
         save(['lastfitness_',num2str(order),'_',num2str(dependent_index),'.mat'],'lastfitness');
                
      end         
          








