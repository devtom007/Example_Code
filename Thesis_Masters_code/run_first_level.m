function run_first_level(settings, set_folder, output_folder)
% settings='fullmasked/ALL_NOSELECTION/p006_03-04'

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% First level analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Parse settings %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
settings = split(settings, '/');
[condition, electrodes, file] = settings{:};

%% Folders %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merged_set_folder = '/data/experiment/eeg_data/merged/';
% single_set_folder = '/data/experiment/eeg_data/preproc/';
% 
% % folder with preprocessed files and output folder 
% if exist([pwd merged_set_folder [file '.set']], 'file')
%     set_folder = merged_set_folder;
% elseif exist([pwd single_set_folder [file '.set']], 'file')
%     set_folder = single_set_folder;
% end

% % folder to save first level files
% output_folder = '/data/experiment/eeg_data/first_lev/kfold';

%% Triggers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unmasked_trials = [11, 21];
fullmask_trials = [31, 41];
stairmask_trials = [51, 61];
present_trials = [11, 31, 51];
absent_trials = [21, 41, 61];

%% Constant first level settings ##########################################

cfg = [];                               % clear the config variable
cfg.model = 'BDM';                      % backward decoding ('BDM') or forward encoding ('FEM')
cfg.raw_or_tfr = 'raw';                 % classify raw or time frequency representations ('tfr')
cfg.nfolds = 10;                        % the number of folds to use
cfg.class_method = 'AUC';               % the performance measure to use
cfg.crossclass = 'no';                  % whether to compute temporal generalization
cfg.channelpool = electrodes;           % the channel selection to use
cfg.resample = 'no';                    % downsample
cfg.save_confidence = 'yes';            % save the confidence scores of the classifier
% this is where the data files are
cfg.datadir = fullfile(pwd, set_folder);        
% this is where data is stored
cfg.outputdir = fullfile(pwd, output_folder, condition);

%% Triggers

if strcmp(condition, 'unmasked')
    cfg.class_spec{1} = cond_string(unmasked_trials, absent_trials); % the first stimulus class
    cfg.class_spec{2} = cond_string(unmasked_trials, present_trials); % the second stimulus class
elseif strcmp(condition, 'fullmasked')
    cfg.class_spec{1} = cond_string(fullmask_trials, absent_trials); % the first stimulus class
    cfg.class_spec{2} = cond_string(fullmask_trials, present_trials); % the second stimulus class
elseif strcmp(condition, 'stairmasked')
    cfg.class_spec{1} = cond_string(stairmask_trials, absent_trials); % the first stimulus class
    cfg.class_spec{2} = cond_string(stairmask_trials, present_trials); % the second stimulus class
elseif strcmp(condition, 'unmaskedStairmasked')    
    cfg.class_spec{1} = [cond_string(stairmask_trials, absent_trials) ',' cond_string(unmasked_trials, absent_trials)];
    cfg.class_spec{2} = [cond_string(stairmask_trials, present_trials) ',' cond_string(unmasked_trials, present_trials)];
end

%% First level

% data filenames
cfg.filenames = {[file '.set']};

% Run first level analysis
adam_MVPA_firstlevel(cfg)

end


%
