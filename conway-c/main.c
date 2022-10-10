#include <SDL2/SDL.h>
#include <time.h>
#include <stdlib.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600
#define WINDOW_TITLE "Conway - Game of Life"
#define APP_CATEGORY SDL_LOG_CATEGORY_CUSTOM

SDL_Window *window = NULL;
SDL_Surface *surface = NULL;
SDL_Renderer *renderer = NULL;

enum {
	Dead,
	Alive
};

int init_sdl()
{
	if(SDL_Init(SDL_INIT_VIDEO) < 0) {
		SDL_LogError(APP_CATEGORY, 
				"Couldn't initialise SDL: %s\n", SDL_GetError());
		return 1;
	}

	window = SDL_CreateWindow(WINDOW_TITLE,
							SDL_WINDOWPOS_UNDEFINED,
							SDL_WINDOWPOS_UNDEFINED,
							SCREEN_WIDTH,
							SCREEN_HEIGHT,
							SDL_WINDOW_ALLOW_HIGHDPI);
	if(window == NULL) {
		SDL_LogError(APP_CATEGORY, 
			"Couldn't create window: %s\n", SDL_GetError());
		return 1;
	}

	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
	if(renderer == NULL) {
		SDL_LogError(APP_CATEGORY, 
			"Couldn't create renderer: %s\n", SDL_GetError());
		return 1;
	}

	return 0;
}

SDL_bool eventLoop()
{
	SDL_Event event;
	while (SDL_PollEvent(&event))
	{
		switch (event.type)
		{
			case SDL_QUIT:
				return SDL_FALSE;
				break;
		}
	}

	return SDL_TRUE;
}

int draw(int grid[])
{
	for(int i = 0; i < SCREEN_WIDTH; i++)
	{
		for(int j = 0; j < SCREEN_HEIGHT; j++)
		{
			int index = j * SCREEN_HEIGHT + i;
			if(grid[index] == Alive)
			{
				SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
			} else
			{
				SDL_SetRenderDrawColor(renderer, 255, 255, 255, 0);
			}
			SDL_RenderDrawPoint(renderer, i, j);
		}
	}

	SDL_RenderPresent(renderer);
}

int countAliveNeighbours(int x, int y, int grid[])
{
	int c = 0;
	int neighbours[8];
	
	neighbours[0] = (SCREEN_WIDTH * (y+1)) + x;
	neighbours[1] = (SCREEN_WIDTH * (y+1)) + x+1;
	neighbours[2] = (SCREEN_WIDTH * (y)) + x+1;
	neighbours[3] = (SCREEN_WIDTH * (y-1)) + x+1;
	neighbours[4] = (SCREEN_WIDTH * (y-1)) + x;
	neighbours[5] = (SCREEN_WIDTH * (y-1)) + x-1;
	neighbours[6] = (SCREEN_WIDTH * (y)) + x-1;
	neighbours[7] = (SCREEN_WIDTH * (y+1)) + x-1;

	for(int i = 0; i < 8; i++)
	{
		int index = neighbours[i];
		if(index < 0 || index >= SCREEN_WIDTH * SCREEN_HEIGHT)
			continue;

		c += grid[index];
	}
	return c;
}


int evolve(int aliveCount, int currentState)
{
	if(aliveCount < 2 || aliveCount > 3)
		return Dead;
	else if(aliveCount == 3)
		return Alive;

	return currentState;
}

int main(int argc, char **argv)
{
	int n = 100; // TODO take from argv
	int gridA[SCREEN_HEIGHT * SCREEN_WIDTH];
	int gridB[SCREEN_HEIGHT * SCREEN_WIDTH];
	int *frontGrid = NULL, *backGrid = NULL;
	SDL_bool isRunning = SDL_TRUE;

	SDL_LogSetPriority(APP_CATEGORY, SDL_LOG_PRIORITY_VERBOSE);
	srand(time(NULL));

	if(init_sdl() > 0) return 1;

	// init random population
	for(int i = 0; i < SCREEN_HEIGHT * SCREEN_WIDTH; i++)
	{
		gridA[i] = rand() % 2;
	}

	//while(isRunning)
	for(; n >= 0; n--)
	{
		frontGrid = (n % 2 == 0) ? gridA : gridB;
		backGrid = (n % 2 == 0) ? gridB : gridA;

		isRunning = eventLoop();

		draw(frontGrid);
		for(int i = 0; i < SCREEN_HEIGHT * SCREEN_WIDTH; i++)
		{
			int x = i % SCREEN_WIDTH,
				y = i / SCREEN_WIDTH;
			
			int aliveCount = countAliveNeighbours(x, y, frontGrid);
			backGrid[i] = evolve(aliveCount, frontGrid[i]);
		}

		//if(--n == 0) isRunning = SDL_FALSE;
		//SDL_Delay(1000);
	}

	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(window);
	SDL_Quit();
	return 0;
}
