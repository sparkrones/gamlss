import numpy as np

list = []

# Arakawa
ix = 3197
iy = 542
# Stung Treng
# ix = 2859
# iy = 764

# 末尾　_hist: 1975-2014
for year in range(1975, 2015):
    f = f"/work/a07/ykimura/2022_LaRC/out/C6-g06M_ACC_hist/outflw{year}.bin"
    dat = np.fromfile(f, "float32").reshape(-1, 1800, 3600)
    dat_max = np.max(dat[:, iy, ix])
    list.append([year, dat_max])

# 末尾　“_y2015-2022”: 2015-2020
for year in range(2015, 2021):
    f = f"/work/a07/ykimura/2022_LaRC/out/C6-g06M_ACC_ssp245_y2015-2022/outflw{year}.bin"
    dat = np.fromfile(f, "float32").reshape(-1, 1800, 3600)
    dat_max = np.max(dat[:, iy, ix])
    list.append([year, dat_max])


# 末尾なし: 2021-2100
for year in range(2021, 2101):
    f = f"/work/a07/ykimura/2022_LaRC/out/C6-g06M_ACC_ssp245/outflw{year}.bin"
    dat = np.fromfile(f, "float32").reshape(-1, 1800, 3600)
    dat_max = np.max(dat[:, iy, ix])
    list.append([year, dat_max])

array = np.array(list)
array.astype('float32').tofile('/work/a06/stakahashi/data/max_y125.bin')