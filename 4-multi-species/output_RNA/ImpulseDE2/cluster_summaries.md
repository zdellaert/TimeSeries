# Species summaries of Impulse DE Clusters (Mfuzz)

## Cluster number choice 

All 3 species had good clustering definition with 6 clusters based on elbow plots. This was set for each species in species_parameters.R

---

## Cross-species cluster comparison

| Pattern              | Pacuta | Mcap | Pcomp | Conserved?     |
|----------------------|--------|------|-------|----------------|
| Early Peak (3h)      | 1      | 3    | 1     | Yes            |
| Early Dip (3h)       | 2      | 2    | -     | Pacuta & Mcap  |
| U-shaped Dip (12h)   | -      | 4    | 2     | Mcap & Pcomp   |
| Sustained Up (3h)    | 4      | -    | 6     | Pacuta & Pcomp |
| Sustained Up (12h)   | 6      | 5    | 3     | Yes            |
| Sustained Down (12h) | 5      | 6    | 5     | Yes            |
| Gradual Down         | 3      | 1    | 4     | Yes            |

**4 patterns conserved across all species; 3 patterns shared by 2 species.**

## Pattern Descriptions

| Pattern                  | Description                                                           |
|--------------------------|-----------------------------------------------------------------------|
| **Early Peak (3h)**      | Sharp increase at 3h, returns toward baseline                         |
| **Early Dip (3h)**       | Sharp decrease at 3h, returns to baseline                             |
| **U-shaped Dip (12h)**   | Gradual decrease from 1h to 12h, returns toward baseline              |
| **Sustained Up (3h)**    | Sharp increase from 1h to 3h, remains elevated through 120h           |
| **Sustained Up (12)**    | Sharp increase from 3h to 12h, remains elevated through 120h          |
| **Sustained Down (12h)** | Gradual or sharp decrease from 1h/3h to 12h, remains low through 120h |
| **Gradual Down**         | Continuous decline from 3h to 120h                                    |

## Pacuta

| Cluster | Pattern               | Peak | Trough | n_genes | Dominant ImpulseDE Response Type |
|---------|-----------------------|------|--------|---------|----------------------------------|
| 1       | Early Peak (3h)       | 3h   | 0h     | 1641    | Transient (1401)                 |
| 2       | Early Dip (3h)        | 0h   | 3h     | 1831    | Transient (1572)                 |
| 3       | Gradual Down          | 1h   | 120h   | 1656    | Monotonous (993)                 |
| 4       | Sustained Up (3h)     | 3h   | 1h     | 1546    | Transient (700)                  |
| 5       | Sustained Down (12h)  | 1h   | 12h    | 1522    | Monotonous (666)                 |
| 6       | Sustained Up (12h) | 24h  | 3h     | 1508    | Monotonous (718)                 |

## Mcap

| Cluster | Pattern               | Peak | Trough | n_genes | Dominant Type    |
|---------|-----------------------|------|--------|---------|------------------|
| 1       | Gradual Down          | 3h   | 120h   | 1172    | Monotonous (656) |
| 2       | Early Dip (3h)        | 120h | 3h     | 843     | Transient (436)  |
| 3       | Early Peak (3h)       | 3h   | 0h     | 863     | Transient (526)  |
| 4       | U-shaped Dip (12h)    | 0h   | 12h    | 908     | Transient (494)  |
| 5       | Sustained Up (12h) | 12h  | 0h     | 949     | Monotonous (597) |
| 6       | Sustained Down (12h)  | 0h   | 12h    | 1146    | Monotonous (795) |

## Pcomp

| Cluster | Pattern               | Peak | Trough | n_genes | Dominant Type    |
|---------|-----------------------|------|--------|---------|------------------|
| 1       | Early Peak (3h)       | 3h   | 1h     | 754     | Transient (490)  |
| 2       | U-shaped Dip (12h)    | 0h   | 12h    | 954     | Transient (502)  |
| 3       | Sustained Up (12h) | 120h | 1h     | 634     | Other (336)      |
| 4       | Gradual Down          | 3h   | 120h   | 1025    | Monotonous (685) |
| 5       | Sustained Down (12h)  | 1h   | 12h    | 1471    | Monotonous (934) |
| 6       | Sustained Up (3h)     | 12h  | 1h     | 903     | Other (458)      |