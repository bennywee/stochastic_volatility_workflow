# from joblib import Parallel, delayed
# from multiprocessing import Pool
import multiprocessing as mp

print("Start")

# def power_n_list(x_list, n):
#     return [x ** n for x in x_list]

def slice_data(data, nprocs):
    aver, res = divmod(len(data), nprocs)
    nums = []
    for proc in range(nprocs):
        if proc < res:
            nums.append(aver + 1)
        else:
            nums.append(aver)
    count = 0
    slices = []
    for proc in range(nprocs):
        slices.append(data[count: count+nums[proc]])
        count += nums[proc]
    return slices

def test(v):
    return(v+1)


nprocs = 2
pool = mp.Pool(processes=nprocs)

inp_lists = slice_data(range(10), nprocs)
multi_result = [pool.apply_async(test, inp) for inp in inp_lists]

# result = [x for p in multi_result for x in p.get()]
print(multi_result)

# def test(v):
#     print(v)

# Parallel(n_jobs=2, backend="multiprocessing")(delayed(test)(i) for i in [1,2,3,4])

# with Pool(2) as p:
    # p.map(test, [1, 2, 3,4,5,6])
    # p.starmap(test, [1, 2, 3,4,5,6])

print("Finish")



