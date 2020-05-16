size_t workSize, parallelCount;
std::vector<std::any> data;
// -----------------------------------------------------------------------

for(size_t i = taskId; i < data.size(); i += parallelCount) {
    // Красиво, но если компилятор туповат, будет много кеш-промахов.
	handle(data[i]);
}

// -----------------------------------------------------------------------

static size_t getBorder(size_t id, size_t parallelCount, size_t taskSize, size_t workSize) {
	return (id < parallelCount - 1) ? (id * taskSize) : workSize;
}
	
auto taskSize = workSize / parallelCount;
for (size_t i = getBorder(taskId, parallelCount, taskSize, data.size()); i < getBorder(taskId+1, parallelCount, taskSize, data.size()); ++i) {
	// Локальность данных соблюдается явно, но выглядит гораздо хуже.
	handle(data[i]);
}

// -----------------------------------------------------------------------

class BorderKeeper {
	
public:
	BorderKeeper(size_t parallelCount, size_t workSize) {
		taskSize = workSize / parallelCount;
	}
	
	size_t getBorder(size_t id) {
		return (id < parallelCount - 1) ? (id * taskSize) : workSize;
}
	
private:
	size_t parallelCount;
	size_t taskSize;
	size_t workSize;
}

auto borders = BorderKeeper(parallelCount, workSize);
for (size_t i = borders.getBorder(taskId); i < borders.getBorder(taskId+1); ++i) {
	// Локальность данных соблюдается явно, выглядит хорошо.
	handle(data[i]);
}
// -----------------------------------------------------------------------