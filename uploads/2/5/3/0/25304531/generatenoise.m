function randimg = generatenoise(imagename, noiseamount)
%% this function generates a random noise patch- it slices the original image and then Shuffle pixels and overlay... 
%% you can vary the proportion noise from 0-1 (i.e. 0-100%). 
img = imread(imagename); %%%%%% 
proportionnoise = noiseamount; %%%%%%% 
imgrect = 300+[0 0 size(img,2) size(img,1)];
nrow = randperm(size(img,1));
ncol = randperm(size(img,2));
rnrow = Shuffle(nrow);
rncol = Shuffle(ncol);
nofrow = round(length(rnrow)*proportionnoise);
nofcol = round(length(rncol)*proportionnoise);
samprnrow = rnrow(1:nofrow);
samprncol = rncol(1:nofcol);
sampnrow = nrow(1:nofrow);
samprcol = ncol(1:nofcol);
randimg = zeros(size(img,1), size(img,2), 4); 
randimg(samprnrow,samprncol,1:3) = img(sampnrow,samprcol,1:3);
randimg(samprnrow,samprncol,4) = 255; 

%% Now you can generate a texture for the noise, for example:
%NoiseTexture=Screen('MakeTexture', w, randimg);

%% After generating a texture for both the original image and noise, 
%% you would then draw both textures and present them. For example:
% Screen('DrawTexture', w, ImageTexture, [], center);
% Screen('DrawTexture', w, NoiseTexture, [], center);