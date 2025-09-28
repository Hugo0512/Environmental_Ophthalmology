%本程序查找矩阵中的简单路径存储在一个矩阵中
clc
clear all
root_dir='D:\FCM2024运算结果\CAO';
cd(root_dir);
subdirs = struct2cell(dir(root_dir));
len=size(subdirs,2);
replace_words={'PM2.5','PM10','SO2','CO','NO2','O3','Termperature','Atmospheric pressure','Humidity','Wind speed','Minimum temperature','Maximum temperature','Visibility','Dew point','Precipitation','Vitreoretinal associated diseases','TAO','Len-associated diseases ','Lacrimal duct-associated diseases',' Uveitis','Glaucoma',' Strabismus','Diseases of ocular surface and its appendages','Ophthalmic nerve-associated diseases',' Ocular trauma','Total cases'};%存储替换的属性名
pathlimit=5;%路径长度限制，不然会组合爆炸


for index0011=1:len
if isempty(strfind(subdirs{1,index0011},'_model.txt'))~=1 && isempty(strfind(subdirs{1,index0011},'all_model.txt'))==1
    barposition=strfind(subdirs{1,index0011},'_');
    
attribute_col_number=str2num(subdirs{1,index0011}(1:barposition(end)-1));
temp_replace_words=[replace_words(1:15) replace_words{attribute_col_number}];


save_path_dir='D:\FCM2024运算结果\CAO\simplepath';
model_name=subdirs{1,index0011};
matrix=load(model_name);%导入数据
result=zeros(16,16);%第二个模型改成10,统计所有的两两因素之间的路径数量
totalsimplepaths={};
    for index1=1:16%第二个模型改成10
        simplepaths={};%存储全部简单路径
        tempdata=matrix(:,index1);%拿出本列
        tempstagesimplepaths={};
        pathlimitindex=1;
        pathlimitindex=pathlimitindex+1;
        for index2=1:numel(tempdata)%找长度为2的简单路径
            if tempdata(index2)~=0 & index2~=index1 
                simplepaths{numel(simplepaths)+1}=[index2 index1];
                tempstagesimplepaths{numel(tempstagesimplepaths)+1}=[index2 index1];
            end
        end
        flagsimplepath={};%存储中间计算结果
        while numel(tempstagesimplepaths)~=0%循环找更长的路径
             flagtempstagesimplepaths={};
             pathlimitindex=pathlimitindex+1;
             if pathlimitindex>pathlimit
                break;
             end
            for index3=1:numel(tempstagesimplepaths)
                tempsimplepathpart1=tempstagesimplepaths{index3};
                tempstart=tempsimplepathpart1(1);%下一阶段的开始点
                tempdata=matrix(:,tempstart);
                 for index2=1:numel(tempdata)%找长度更长的简单路径
                     %判断此起点是否与后面节点重复
                        tempresult=find(index2==tempsimplepathpart1);
                        if tempdata(index2)~=0 & index2~=index1 & isempty(tempresult)==1
                            simplepaths{numel(simplepaths)+1}=[index2 tempsimplepathpart1];
                            flagtempstagesimplepaths{numel(flagtempstagesimplepaths)+1}=[index2 tempsimplepathpart1];
                        end
                 end 
            end
            if numel(flagtempstagesimplepaths)~=0
                tempstagesimplepaths=flagtempstagesimplepaths;
            
            else
                break;
            end
        end
        totalsimplepaths{index1}=simplepaths;%此变量存储的是所有的简单路径
        
            %break跳出到此处
            %统计每个起点到终点的数量
            temppaths=totalsimplepaths{index1};
            %将重点是index1的简单路径存xls文件
            maxlength=0;%存储最大路径长度
            for itemindex=1:numel(temppaths)
                specificpath=temppaths{itemindex};
                if numel(specificpath)>maxlength
                    maxlength=numel(specificpath);
                end
            end
            save_result={};
            for itemindex=1:numel(temppaths)
               realpath=temppaths{itemindex};
                weights=[];
                for stepindex=1:numel(realpath)-1
                    weights=[weights matrix(realpath(stepindex),realpath(stepindex+1))];
                end
                %将path中的数字换成名字
                
                realpath_replacename={};
                for realpathindex=1:numel(realpath)
                    realpath_replacename=[realpath_replacename temp_replace_words{realpath(realpathindex)}];
                end
                fillzeropath=[realpath_replacename num2cell(zeros(1,maxlength-numel(temppaths{itemindex}))) prod(weights)];
                
                
                save_result=[save_result; fillzeropath];
            end
            save_file_name=[save_path_dir,'\',strrep(model_name,'.txt',''),'_',num2str(index1),'.xlsx'];
            %save_result=num2cell(save_result);
            xlswrite(save_file_name,save_result);
            for index9=1:16%第二个模型改成10
                for index10=1:numel(temppaths)
                    littlepath=temppaths{index10};
                    if littlepath(1)==index9 & littlepath(end)==index1
                        result(index9,index1)=result(index9,index1)+1;
                    end
                end
            end
    end
    last_save_file_name=[save_path_dir,'/',strrep(model_name,'.txt',''),'_count','.txt'];
    save(last_save_file_name,'result','-ascii')
end
end