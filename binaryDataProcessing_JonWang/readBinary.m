function X = readBinary(filename,varargin)
% readBinary(filename) - Reads data to a double vector
% readBinary(filename,prec) - Reads data to a PREC-precision vector
% readBinary(filename,s) - Reads data to a double s(1) x s(2) matrix
% readBinary(filename,s,prec) _ Reads data to a PREC-precision matrix
% This method assumes that the binary file was written with sequential
% access by GNU Fortran.  The information is written in records, with the
% largest possible record being intmax('int32').  If the file is larger
% than this number, than it has more than one record.  Each record is padded
% with a header and footer of 4 bytes each (a 32-bit integer) that is included
% in the size of the record (although variables within readBinary will
% focus on the size of the contents of the record, not the actual size of
% the record).

   if (isempty(varargin))
      s = [];
      precision = 'double';
   elseif (length(varargin)==1)
      if (ischar(varargin{1}))
         s = [];
         precision = varargin{1};
      elseif (isreal(varargin{1}))
         s = varargin{1};
         precision = 'double';
      else
         fprintf('ERROR: Unknown option.');
         keyboard;
      end
   elseif (length(varargin)==2)
      if (isreal(varargin{1}) && ischar(varargin{2}))
         s = varargin{1};
         precision = varargin{2};
      else
         fprintf('ERROR: Unknown option.');
         keyboard;
      end
   else
      fprintf('ERROR: Unknown option.');
      keyboard;
   end
   
   % Determine size of each element in bytes
   if (strcmp(precision,'int32'))
      elementBytes = 4;
   elseif (strcmp(precision,'double'))
      elementBytes = 8;
   else
      fprintf('ERROR: Unknown precision: %s\n',precision);
      keyboard;
   end
   
   % Open file
   unit = fopen(filename);
   if (unit < 0)
      fprintf('    FILE CANNOT BE OPENED: %s\n',filename);
      X = [];
      return;
   end
   
   % Determine number of elements per record and size of each record
   padBytes = 4; % int32 padding between each record
   fileInfo = dir(filename); % struct with file information
   fileBytes = fileInfo.bytes; % size of file in bytes
   recordBytesMax = double(intmax('int32'))-2*padBytes; % maximum record size (excluding padding)
   recordBytes = min(fileBytes-2*padBytes,... % size of each record (excluding padding)
      recordBytesMax-mod(recordBytesMax,elementBytes));
   
   % This is the crucial information
   nRecords = ceil(fileBytes/(recordBytes+2*padBytes));
   nElementsPerRecord = recordBytes/elementBytes;
   if (nRecords == 1)
      nElementsLastRecord = nElementsPerRecord;
   else
      nElementsLastRecord = max(0,mod(fileBytes,recordBytes+2*padBytes) - 2*padBytes)/elementBytes;
   end
   nElements = nElementsPerRecord*(nRecords-1) + nElementsLastRecord;

   % Initialize output array; check dimensions.
   if (~isempty(s))
      if (prod(s) == nElements)
         X = zeros(s);
      else
         fprintf('   *WARNING* Inconsistent sizes: %s\n',filename);
         keyboard;
      end
   else
      if (fileBytes == nElements*elementBytes + 2*padBytes*nRecords)
         X = zeros(nElements,1);
      else
         fprintf('   *WARNING* Inconsistent sizes: %s\n',filename);
         keyboard;
      end
   end
   
   % Read each record
   for i = 1:nRecords-1
      fread(unit,1,'int32');
      X(1+(i-1)*nElementsPerRecord:i*nElementsPerRecord) = fread(unit,nElementsPerRecord,precision);
      fread(unit,1,'int32');
   end
   fread(unit,1,'int32');
   X(1+(nRecords-1)*nElementsPerRecord:end) = fread(unit,nElementsLastRecord,precision);
   fread(unit,1,'int32');
   
   % Check for leftover elements.
   if (~isempty(fread(unit)))
      fprintf('    *WARNING#* INCOMPLETE READ: %s\n',filename);
      keyboard;
   else
      fprintf('    File Read: %s\n',filename);
   end
   
   fclose(unit);
   
end