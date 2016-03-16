#include <SDL2/SDL.h>
#include <SDL2/SDL_audio.h>
#include <stdio.h>


int samplerate = 4000; // 105(3*5*7) * 2^9 ;  try 44100 too 
float buffersize = 0.1; // how large buffer is, in seconds
float angvel = 2*M_PI*440; // frequency, or how many cycles per second
int usesound; // if sound is working or not (skips if not working)
int audiodeviceid; // id of device returned by SDL_OpenAudioDevice

int array[30000];
int arraysize;
int arraymin;
int arraymax;
int arrayloc = 0;


typedef struct
{
	float theta;
} audiodata;


void audiocallback(void* data, Uint8* stream, int len)
{
	audiodata* ad = (audiodata*)data;
	Sint16* samples = (Sint16*)stream;
	int samplecount = len/2;
	int i;
	for(i=0;i<samplecount;i++)
	{
		samples[i] = (((float)(array[arrayloc]-arraymin))/(arraymax-arraymin))*60000.0-30000.0;
		int olda = arrayloc;
		arrayloc++;
		//arrayloc%= arraysize;
		//arrayloc%= 10;
		if(arrayloc>arraysize)
		{
			//arrayloc = 1000;
			arrayloc = 36;
		}

		int di = array[arrayloc] - array[olda];
		int jump = 6;//xx
		if(di>jump || di < -jump)
		{
			printf("%d\n",arrayloc);
		}

	}
}

void audioinit()
{
	audiodata* ad = (audiodata*)malloc(sizeof(audiodata));
	ad->theta = 0;

	SDL_AudioSpec want;
	want.freq = samplerate; // samples per second
	want.format = AUDIO_S16SYS; // data type of sample
	want.channels = 1; // 1 for mono, 2 for stereo, etc
	want.samples = samplerate*buffersize; // how many samples in a buffer
	want.callback = audiocallback;
	want.userdata = (void*)ad;

	SDL_AudioSpec have;

	// Open sdl audio device and set usesound accordingly
	audiodeviceid = SDL_OpenAudioDevice(0,0,&want,&have,SDL_AUDIO_ALLOW_ANY_CHANGE);
	if(audiodeviceid == 0) // error while opening
	{
		usesound = 0;
		printf("Error opening audio: %s\n",SDL_GetError());
	}
	else // opened without error
	{
		if(have.format!=want.format)
		{
			printf("Audio format difference: got SDL_AudioFormat code %d\n",have.format);
		}
		if(have.freq!=want.freq)
		{
			printf("Audio frequency difference: requested %d but got %d\n",
					want.freq,have.freq);
		}
		if(have.channels!=want.channels)
		{
			printf("Audio channels difference: requested %d but got %d\n",
					want.channels,have.channels);
		}
		usesound = 1;
	}

	
}

int main(int argc, char* argv[])
{
	SDL_Init(SDL_INIT_AUDIO);

	audioinit();

	FILE* file = fopen("romans","r");
	char string[30000];
	char* p = string;

	int i = 0;
	char c;
	while((c=getc(file))!=EOF)
	{
		if(c==',') c = ' ';
		p[i++] = c;
	}
	char* end;
	arraysize = 0;
	arraymin = 1000000;
	arraymax = 0;
	for(i = strtol(p,&end,10); p!= end; i=strtol(p,&end,10))
	{
		p = end;
		array[arraysize] = i;
		arraysize++;
		if(i<arraymin) arraymin = i;
		if(i>arraymax) arraymax = i;
	}

	for(i=0;i<arraysize;i++)
	{
		printf("%d\n", array[i]);
	}
	printf("min: %d  max: %d  size: %d\n", arraymin, arraymax, arraysize);

	if(usesound)
	{
		SDL_PauseAudioDevice(audiodeviceid, 0);
		printf("Playing sound!  Entering endless while loop, hit cntrl+c to cancel\n");
		while(1){}
	}


	return 0;
}
