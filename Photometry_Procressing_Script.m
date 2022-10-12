close all; clear all; clc; 

%% Define parameters
MagEntry = 1;
PelletDrop = 2;
CS_1P = 3;
CS_3P = 4;
FC_Left_Correct = 5;
FC_Right_Incorrect = 6;
Free_Choice_Left = 7;
FC_Right_Correct = 8;
FC_Left_Incorrect = 9;
Free_Choice_Right = 10;
FC_Left_Cue = 11;
FC_Right_Cue = 12;
Free_Choice_Cues = 13;
ForceC_1_correct = 21;
ForceC_3_incorrect = 22;
FreeC_1 = 23;
ForceC_3_correct = 24;
ForceC_1_incorrect = 25;
FreeC_3 = 26;
CS_1P_MT = 31;
CS_3P_MT = 32;
CS_both = 33;
ITI = 40;

% FileNum = 22; % Change this number based on which file you want
% FileNum = FileNum+2; %start at row 3 because rows 1 and 2 are '.' and '..' or something equally useless, not the data file we want

% Make some pretty colors for later plotting
% <http://math.loyola.edu/~loberbro/matlab/html/colorsInMatlab.html>
red = [0.8500, 0.3250, 0.0980];
green = [0.4660, 0.6740, 0.1880];
cyan = [0.3010, 0.7450, 0.9330];
gray1 = [.7 .7 .7];
gray2 = [.8 .8 .8];
black = [0 0 0];
blue = [0 0.4470 0.7410];
magenta = [1 0 1];

%% Set folders for data import and create folder lists
addpath '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/TDTbin2mat';
addpath '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Code';
addpath '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Event_Timestamps/Pav_Task'
%Synapse data
BLOCKPATH = '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Data/Magnitude_Task_D7'; %Synapse file path
cd(BLOCKPATH);
SynapseFiles=GetSubDirsFirstLevelOnly(BLOCKPATH);

EventDataFolder = '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Event_Timestamps/Magnitude_Task/D7'; %CSV event data filepath
cd(EventDataFolder);
EventCSVs=dir('*.csv');

%save folder and name of output file
Phase_ind = find(BLOCKPATH == '/',1,'last');
Phase=BLOCKPATH(Phase_ind:end);
save_fold = '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data'; %filepath for save location
filename=strcat(save_fold,Phase,'.csv'); %name of output file


%% Start loop for each rat/day
column=2; %starts at two to include time column
for s=1:4 % length(SynapseFiles) %this assumes first data file starts at 3 in list
    %Import data from Synapse and MEDPC
    BLOCKPATH = '/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Data/Magnitude_Task_D7'; %Synapse file path
    BLOCKPATH = fullfile(BLOCKPATH, SynapseFiles{s}); 
    data = TDTbin2mat(BLOCKPATH, 'TYPE', {'epocs','streams'});
    EventData = readmatrix(EventCSVs(s).name);

    disp('CSV'); disp(EventCSVs(s).name)
    disp('TDT'); disp(SynapseFiles{s})
    IDs=split(EventCSVs(s).name);
    RatID1=IDs{1,1}; RatID2=IDs{2,1}; DayID=strtok(IDs{3,1},'.');
    for j=1:1 %2 for the number of boxes
        if j==1
            Box = 'Box1';
            GCAMP = 'x465B';
            ISOS = 'x405B';
            onsets_work = EventData(:,1);
            eventstamp_work = EventData(:,2);
            disp(RatID1); disp(DayID)
            RatID=single(RatID1);
        else 
            Box = 'Box3';
            GCAMP = 'x465C';
            ISOS = 'x405C';
            onsets_work = EventData(:,3);
            eventstamp_work = EventData(:,4);
            disp(RatID2); disp(DayID)
            RatID=single(RatID2);
        end
        fs = data.streams.(GCAMP).fs;
        fs_onset = 1000;
        onsets = onsets_work(eventstamp_work < 4999); %remove dummy events (5000 = N/A)
        eventstamp = eventstamp_work(eventstamp_work < 4999); %remove dummy events (5000 = N/A)
        
        %% Zero event data to Synapse data
        first = data.epocs.(Box).onset(1,1);
        last = data.epocs.(Box).onset(end,1); %add ITI (40") to last event onset to account for final ITI and 
        % allow us to include final trial for peri-event. For some reason I need to add 80 as well for the timestamps to line up??
        start_fin = [round(first),round(last)];
        disp('Session Start & Finish'); disp(start_fin);
        
        %% Create index for start and end of session and select epoc of interest
        first = round(first*1000); %turn start and finish epocs into integers to rezero streams data in next session
        %last = round(last*1000);
        
        %Isolate first mag entries after pellet drops
%         pel_onsets=onsets(eventstamp==PelletDrop);
%         mag_onsets=onsets(eventstamp==MagEntry);
%         collection = 10; %this cuts off collection time to 10s from pellet drop
%         first_mag=zeros(length(pel_onsets),1);
%         for i=1:length(pel_onsets)
%             pel_end = pel_onsets(i)+collection;
%             pel_wind = round((linspace(pel_onsets(i),pel_end,((collection*100)-1))),2);
%             mag_ind=ismember(mag_onsets,pel_wind);
%             all_mags=mag_onsets(mag_ind);
%             if sum(all_mags)>0
%                 first_mag(i,1)=all_mags(1,1);
%             else
%                 first_mag(i)=5000; %for any zero values, there are no corresponding mag entries within 10s
%             end
%         end
% 
%         first_mag=first_mag(first_mag<5000);

        data.epocs.Event.name = [];
        data.epocs.Event.onset = onsets(eventstamp == CS_3P_MT); %make epoc onset array of all times where event codes = event
        data.epocs.Event.offset = data.epocs.Event.onset + 0.01; 
        data.epocs.Event.data = ones(numel(data.epocs.Event.onset),1);
        data.epocs.Event.typeStr = data.epocs.(Box).typeStr;
        Box = 'Event';
        
        %% make a time vector, clip stream data according to session start and finish, and plot
        
        time_GCAMP = (1:length(data.streams.(GCAMP).data))/fs;
        index_START = single(round(time_GCAMP(1,first)*1000)); %find the index of the first epoc onset which is when the MEDPC program started
        %index_END = single(round(time_GCAMP(1,last)*1000)); %find the index of the last epoc onset
        data.streams.(GCAMP).data = data.streams.(GCAMP).data(index_START:end); %only include streams data from MEDPC program start onwards
        data.streams.(ISOS).data = data.streams.(ISOS).data(index_START:end);
        time_GCAMP = (1:length(data.streams.(GCAMP).data));
        time_graph = time_GCAMP/fs;
        
        figure('Position',[100, 100, 800, 400])
        hold on;
        p1 = plot(time_graph, data.streams.(GCAMP).data,'color',blue,'LineWidth',2);
        p2 = plot(time_graph, data.streams.(ISOS).data,'color',magenta,'LineWidth',2);
        title('Raw Demodulated Responses','fontsize',16);
        ylabel('mV','fontsize',16);
        axis tight;
        legend([p1 p2], {'GCaMP','Isos'});

        %% local downsampling
%         N = 10; % multiplicative for downsampling
%         data.streams.(GCAMP).data = arrayfun(@(i)...
%             mean(data.streams.(GCAMP).data(i:i+N-1)),...
%             1:N:length(data.streams.(GCAMP).data)-N+1);
%         data.streams.(ISOS).data = arrayfun(@(i)...
%             mean(data.streams.(ISOS).data(i:i+N-1)),...
%             1:N:length(data.streams.(ISOS).data)-N+1);
% 
%         
%         fs = data.streams.(GCAMP).fs/N;
%         fs_onset = fs_onset/N;
%         time_GCAMP = time_GCAMP(10:N:end);
%         time_GCAMP = time_GCAMP(1:length(data.streams.(GCAMP).data))/N;
       
        
        %% Artifact removal for disconnections
        % bls = polyfit(time_GCAMP,data.streams.(ISOS).data,1);
        cutoff_GCAMP = 3;
        cutoff_ISOS = 3;
        mean_ISOS = mean(data.streams.(ISOS).data);
        ARTIFACT_ISOS = cutoff_ISOS*std(data.streams.(ISOS).data); %change this to increase or decrease window for artifact removal
        mean_GCAMP = mean(data.streams.(GCAMP).data);
        ARTIFACT_GCAMP = cutoff_GCAMP*std(data.streams.(GCAMP).data);

        arti_1 = data.streams.(ISOS).data>(mean_ISOS+ARTIFACT_ISOS);
        arti_2 = data.streams.(ISOS).data<(mean_ISOS-ARTIFACT_ISOS);
        arti_3 = data.streams.(GCAMP).data>(mean_GCAMP+ARTIFACT_GCAMP);
        arti_4 = data.streams.(GCAMP).data<(mean_GCAMP-ARTIFACT_GCAMP);
        arti_all = arti_1 | arti_2|arti_3|arti_4;
        data.streams.(ISOS).data = data.streams.(ISOS).data(1,~arti_all);
        data.streams.(GCAMP).data = data.streams.(GCAMP).data(1,~arti_all);
        arti_ind=time_GCAMP(arti_all);
        time_GCAMP = time_GCAMP(~arti_all);
        time_graph = time_GCAMP/fs;

        timeArti = sum(arti_all)/fs;      
        disp ('Duration of Artifacts'); disp(timeArti);
                
        
        figure('Position',[100, 100, 800, 400])
        hold on;
        p1 = scatter(time_graph, data.streams.(GCAMP).data,1,blue,'filled');
        p2 = scatter(time_graph, data.streams.(ISOS).data,1,magenta,'filled');
        title('Raw Demodulated Responses','fontsize',16);
        ylabel('mV','fontsize',16);
        axis tight;
        legend([p1 p2], {'GCaMP','Isos'});
                

        
        %% basic dFF
        bls = polyfit(data.streams.(ISOS).data,data.streams.(GCAMP).data,1);
        Y_fit_all = bls(1) .* data.streams.(ISOS).data + bls(2);
        Y_dF_all = data.streams.(GCAMP).data - Y_fit_all; %dF (units mV) is not dFF
        
        dFF = 100*(Y_dF_all)./Y_fit_all;
        std_dFF = std(double(dFF));
        
        %% make some epoc events on plot
        PELLET_on = data.epocs.Event.onset;
        PELLET_off = data.epocs.Event.offset;
        PELLET_x = reshape(kron([PELLET_on, PELLET_off], [1, 1])', [], 1);
        sz = length(PELLET_on);
        d = data.epocs.Event.data';
        y_scale = 10; %adjust according to data needs
        y_shift = -20; %scale and shift are just for aesthetics
        PELLET_y = reshape([zeros(1, sz); d; d; zeros(1, sz)], 1, []);
        
        figure('Position',[100, 100, 800, 400]);
        p1 = plot(time_graph, dFF, 'Color',green,'LineWidth',2); hold on;
        p2 = plot(PELLET_x, y_scale*(PELLET_y) + y_shift,'color',cyan,'LineWidth',2);
        title('Detrended, y-shifted dFF','fontsize',16);
        legend([p1 p2],'GCaMP','Pellets');
        axis tight
        %PELLET_y = reshape([zeros(1, sz); d; d; zeros(1, sz)], 1, []);
        
        %% combine two TTLs into one pellet event 
        EVENT = 'EVENT';
        data.epocs.(EVENT).name = EVENT;
        data.epocs.(EVENT).onset = [];
        data.epocs.(EVENT).offset = [];
        data.epocs.(EVENT).typeStr = data.epocs.(Box).typeStr;
        data.epocs.(EVENT).data = [];
        
        %% find big 'event bout' gaps
        PELLET_on_diff = diff(data.epocs.(Box).onset);
        BOUT_TIME_THRESHOLD = 2; % example bout time threshold, in seconds
        pel_diff_ind = find(PELLET_on_diff >= BOUT_TIME_THRESHOLD);
        
        % Note that for speed the previous section could be replaced with these three lines
        data.epocs.(EVENT).onset = data.epocs.Event.onset([1; pel_diff_ind+1]);
        data.epocs.(EVENT).offset = data.epocs.Event.offset([pel_diff_ind; end]);
        data.epocs.(EVENT).data = ones(1, length(data.epocs.(EVENT).onset))';
        
        MIN_PELLET_THRESH = 1; % four licks or more make a bout
        licks_array = zeros(length(data.epocs.(EVENT).onset),1);
        for i = 1:length(data.epocs.(EVENT).onset)
            % Find number of licks in licks_array between onset ond offset of
            % Our new lick BOUT (LICK_EVENT)     
            licks_array(i) = numel(find(data.epocs.(Box).onset >=...
                data.epocs.(EVENT).onset(i) & data.epocs.(Box).onset <=...
                data.epocs.(EVENT).offset(i)));
        end
        
        %remove thrown out events
        data.epocs.(EVENT).onset((licks_array < MIN_PELLET_THRESH)) = [];
        data.epocs.(EVENT).offset((licks_array < MIN_PELLET_THRESH)) = [];
        data.epocs.(EVENT).data((licks_array < MIN_PELLET_THRESH)) = [];
        
        
        %make a continuous event to plot newly made bouts and plot
        PELLET_EVENT_on = data.epocs.(EVENT).onset;
        PELLET_EVENT_off = data.epocs.(EVENT).offset;
        PELLET_EVENT_x = reshape(kron([PELLET_EVENT_on,PELLET_EVENT_off],[1, 1])',[],1);
        sz = length(PELLET_EVENT_on);
        d = data.epocs.(EVENT).data';
        PELLET_EVENT_y = reshape([zeros(1, sz); d; d; zeros(1, sz)], 1, []);
        
        figure; 
        p1 = plot(time_graph, dFF,'Color',green,'LineWidth',2);
        hold on;
        p2 = plot(PELLET_EVENT_x, y_scale*(PELLET_EVENT_y) + y_shift,...
            'color',cyan,'LineWidth',2);
        title('Detrended, y-shifted dFF','fontsize',16);
        legend([p1 p2],'GCaMP', 'Pellet Drop');
        axis tight
        
        
        %% Peri Event Analysis
        BLStart = 5; % x seconds before event to start baseline window
        BLFin = 1; % x seconds before event to finish baseline window
        Peak_Start = 1; % x seconds after PreWind to start peak window
        Peak_Fin = 3; % x seconds after Prewind to end peak window
        PreWind = 1; % x seconds before event onset
        PostWind = 30; % x seconds after event
                
        %Convert timepoints to indices for snipping dff data
        PreWind2 = round(PreWind*fs);
        PostWind2 = round(PostWind*fs);
        BLStart2 = round(BLStart*fs); 
        BLFin2 = round(BLFin*fs);
        Peak_Start2 = round(Peak_Start*fs);
        Peak_Fin2 = round(Peak_Fin*fs);
        
        % pre allocate memory
        EventTS = round(data.epocs.(EVENT).onset*fs);
        ts = time_GCAMP';
        CSTS=round(-PreWind2:PostWind2)/fs; %assumes "event period" starts before event occurs
        CSTSbase=round(-BLStart2:-BLFin2)/fs; %assumes "event period" starts before event occurs
                       
        %Find indices for stream data when events occurred              
        [sharedvals,CSidx]=intersect(ts,EventTS,'stable');
                        
%         %Find Time=0 for the event within the photometry data
%         for i=1:length(EventTS)
%                  [MinVal, CSidx(i,1)]=min(abs(ts(:,1)-EventTS(i))); %CSidx is a list of event timestamps
%         end
        
        %Remove trials/events that contain artifacts
        remove=zeros(length(CSidx),1);
        for i=1:length(CSidx)
            Trial_Wind=((CSidx(i)-BLStart2):(CSidx(i)+PostWind2)); %create trial time window
            Arti_check=ismember(Trial_Wind,arti_ind); %logical array with 1's present if any artifacts found in trial window
            if sum(Arti_check)>0
                remove(i,1)=1;
            end
        end
        
        skipped=CSidx(remove==1); %list of removed trials
        CSidx=CSidx(remove==0); %list of valid trials

        dff_event=zeros(length(CSidx),length(CSTS),1);
        dff_base=zeros(length(CSidx),length(CSTSbase),1);
        dff_snips=zeros(length(CSidx),length(CSTS),1);
        peak_snips=zeros(length(CSidx),1,1);
       
        %Create peri-event dff snips
        for i=1:length(CSidx)
                dff_event(i,:)=dFF((CSidx(i)-(PreWind2)):(CSidx(i)+(PostWind2))); 
                dff_base(i,:)=dFF((CSidx(i)-(BLStart2)):(CSidx(i)-(BLFin2)));
                dff_snips(i,:)=(dff_event(i,:)-mean(dff_base(i,:)))./std(dff_base(i,:)); %Z-Score
                peak_snips(i,1)=max(dff_snips(i,(Peak_Start2):(Peak_Fin2)));
        %     figure;
        %     plot(CSTS,dff_snips(i,:));
        %     hold on; pause
        end
         
        
        % %% Obtain the DeltaF/F for each event window (Barker PETH example)
        %     Ch465=data.streams.(GCAMP).data';
        %     Ch405=data.streams.(ISOS).data';
        %     counter=1;
        %     CSTS=round(-PreWind2:PostWind2)./fs; %assumes "event period" starts before event occurs
        %     CSTSbase=round(-BLStart2:-BLFin2)./fs; %assumes baseline period is prior to event
        % 
        %    %pre-allocate memory for speed
        %     DF_Event=zeros(length(CSTS),length(CSidx),1);
        %     DF_F=zeros(length(CSTS),length(CSidx),1);
        %     DF_Base=zeros(length(CSTSbase),length(CSidx),1);
        %     DF_F_Base=zeros(length(CSTSbase),length(CSidx),1);
        %     DF_ZScore=zeros(length(CSTS),length(CSidx),1);
        %     DF=zeros(length(CSTS),length(CSidx),1);
        %     Peak_ZScore=zeros(length(CSTS),length(CSidx),1);
        %     DFF_slide=zeros(length(CSTS),length(CSidx),1);
        %     for i=1:length(CSidx)
        %         %%NEED TO CHECK IF INDEX IS <0 or GREATER THAN LENGTH OF DELTA490, NOT
        %         %%IF THE VALUE IS LESS THAN 0 OR GREATER THAN THE MAX VALUE OF DELTA490
        %         if CSidx(i)-(BLStart2)<=0 || CSidx(i)+PostWind2 > length(ts)
        %         else
        %             %Obtain data within baseline and event window for 405
        %             %and 490 channels.
        % %             CSBL(1,:)=Ch465((CSidx(i)-(BLStart2)):(CSidx(i)-(BLFin2))); 
        % %             CSBL2(1,:)=Ch405((CSidx(i)-(BLStart2)):(CSidx(i)-(BLFin2)));
        % % %             CS405(1,:)=Ch405((CSidx(i)-PreWind2):(CSidx(i)+PostWind2)); 
        % % %             CS465(1,:)=Ch465((CSidx(i)-PreWind2):(CSidx(i)+PostWind2)); 
        % % 
        % %             %Smooth to eliminate high frequency noise.
        % %             F465=smooth(CS465,0.002,'lowess');  %Was 0.002- DJB
        % %             F405=smooth(CS405,0.002,'lowess');
        % %             F465CSBL=smooth(CSBL,0.002,'lowess');  %Was 0.002- DJB
        % %             F405CSBL=smooth(CSBL2,0.002,'lowess');
        % % 
        % % 
        % %             %Scale and fit data
        % %             bls=polyfit(F405(1:end),F465(1:end),1);
        % %             blsbase=polyfit(F405CSBL(1:end),F465CSBL(1:end),1);
        % %             Y_Fit=bls(1).*F405+bls(2);
        % %             Y_Fit_base=blsbase(1).*F405CSBL+blsbase(2);
        % 
        %             %Center data and generate Delta F/F (DF_F) by dividing
        %             %event window by median of baseline window.
        %             DF_Event(:,i)=(F465(:,1)-Y_Fit(:,1));
        %             DF_F(:,i)=(100*DF_Event(:,i))./(Y_Fit); %Delta F/F
        %             DF_F(:,i)=DF_F(:,i)-DF_F(1,i); %zero to first point
        %             
        %             DF_Base(:,i)=(F465CSBL(:,1)-Y_Fit_base(:,1));
        %             DF_F_Base(:,i)=((DF_Base(:,i)*100)./(Y_Fit_base));
        %             
        %             DF_ZScore(:,i)=(DF_F(:,i)-mean(DF_Base(:,i)))./std(DF_Base(:,i),1); %Z-Score
        %             %DF_ZScore(:,i)=(DF_Event(:,i)-median(DF_Base(:,i)))./mad(DF_Base(:,i),1); %Z-Score
        %             %DF_Base(:,i)=DF_Base(:,i)-DF_Base(1,i); %zero to first point
        %             %DF_Event(:,i)=DF_Event(:,i)-DF_Event(1,i); %zero first point
        %             Peak_ZScore(1,i)=max(DF_ZScore((Peak_Start):(Peak_Fin),i)); %max Z-score during peak window
        % %             DFF_slide(:,counter)=(100*(F465(:,1)-Y_Fit_base(:,1)))/(Y_Fit_base); %sliding DFF??
        %             counter=counter+1;
        %            %Plot Function to compare Robust Z-Score to DF/F            
        %             figure; plot(CSTS,DF_F(:,i),'color',red); hold on; 
        %             %plot(CSTSbase,DF_Base(:,i),'color',gray1);
        %             pause
        %             %%% Clearing variables to reset for the next trial        
        %              clear CS405 CS465 F465 F405 bls Y_Fit Y_Fit_base blsbase CSBL CSBL2
        % 
        %         end
        %     end
        
        % %% very sloppy artifact removal
        % greater than 15 dFF - need to graph the peri-event line graph first to
        % determine what an appropriate artifact level is - arbitrary
        Artifact = 15;
        allSignals = dff_snips;
        row = zeros(length(allSignals),1);
        for i = 1:size(allSignals,1)
            if isempty(find(allSignals(i,:) > Artifact,1)) && isempty(find(allSignals(i,:) < -(Artifact),1))
                row(i) = i; 
            end
        end
        allSignals = allSignals(find(row ~= 0),:);
        
        %ratIDs(rats,1)=RatID; %not working currently, still need to figure
        %out why
        masterdata(column,:) = mean(allSignals);
        masterdata(column+1,:) = std(allSignals);
        masterdata(column+2,:) = mean(peak_snips);
        
%         mean_ID=strcat(RatID,'-','mean');
%         SD_ID=strcat(RatID,'-','SD');
%         peak_ID=strcat(RatID,'-','peak');
%         labels={mean_ID,SD_ID,peak_ID};
%         labels(column,:)=string(labels);


        % Make a time vector snippet for peri-events
        peri_time = (1:length(masterdata(column,:)))/fs - PreWind;
               
        %% plot peri-event data
        % Make a standard deviation fill for mean signal
        figure('Position',[100, 100, 600, 750])
        subplot(2,1,1)
         xx = [peri_time, fliplr(peri_time)]; %fliplr(CSTS)
         yy = [masterdata(column,:) + masterdata(column+1,:),...
             fliplr(masterdata(column,:) - masterdata(column+1,:))];
         h = fill(xx, yy, 'r'); % plot this first for overlay purposes
        hold on;
        set(h, 'facealpha', 0.25, 'edgecolor', 'none');
        
        % Set specs for min and max value of event line.
        % Min and max of either std or one of the signal snip traces
        linemin = min(min(min(allSignals)),min(yy));
        linemax = max(max(max(allSignals)),max(yy));
        
        % Plot the line next
        l1 = line([0 0], [linemin, linemax],...
            'color',black, 'LineStyle', '-', 'LineWidth', 2);
        %l2 = line([5 5], [linemin, linemax],...
           %'color',black, 'LineStyle', '-', 'LineWidth', 2);
        % Plot the signals and the mean signal
        p1 = plot(peri_time, allSignals, 'color', gray1);
        p2 = plot(peri_time, masterdata(column,:), 'color', red, 'LineWidth', 2);
        hold off;
        
        % Make a legend and do other plot things
        legend([l1,p2,p1(1),h],...
            {'NP','Trace','Trial Traces','SD Trace'},...
            'Location','northeast');
        title_text=[RatID '- Peri-Event Trace'];
        title(title_text,DayID,'fontsize',16);
        ylabel('\DeltaF/F','fontsize',16);
        axis tight;
        % Make an invisible colorbar so this plot aligns with one below it
        temp_cb = colorbar('Visible', 'off');
        
        % Heat map
        subplot(2,1,2)
        imagesc(peri_time, 1:size(allSignals,1), allSignals); % this is the heatmap
        set(gca,'YDir','normal') % put the trial numbers in better order on y-axis
        colormap('jet') % colormap otherwise defaults to perula
        title_text=[RatID '- Pellet Heat Map'];
        title(title_text,DayID,'fontsize',16)
        ylabel('Trial Number','fontsize',16)
        xlabel('Seconds from pellet drop','fontsize',16)
        cb = colorbar;
        ylabel(cb, 'dFF','fontsize',16)
        axis tight;
        
        column=column+3; %counts the sessions once finished analysing each session's data
    end

end
masterdata(1,:)=peri_time;
masterdata = masterdata';
writematrix(masterdata,filename)