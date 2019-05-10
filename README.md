# Club-Mahindra-DataOlympics-Hackathon
Hackathon is conducted jointly by Club Mahindra and Analytics Vidhya from May 3-5, 2019.
Club Mahindra presents [DataOlympics](https://datahack.analyticsvidhya.com/contest/club-mahindra-dataolympics/) in association with Analytics Vidhya. DataOlympics reflects the data-driven culture we have in Club Mahindra where we take up the most critical and impactful business challenges and try to solve them using data insights and advanced predictive models. In our journey to become a data-driven decision making organization, we invite you to DataOlympics, our flagship hackathon to identify the best talents in the industry to work on challenging business problems.


## Problem Statement
### Food & Beverages Spend Prediction in Club Mahindra Resorts
Club Mahindra (Club M) makes significant revenue from Food and Beverages (F&B) sales in their resorts. The members of Club M are offered a wide
variety of items in either buffet or À la carte form. Following are some benefits that the model to predict the spend by a member in their next visit to a resort will bring:
1. Predicting the F&B spend of a member in a resort would help in improving the pre-sales during resort booking through web and mobile app
2. Targeted campaigns to suit the member taste and preference of F&B 
3. Providing members in the resort with a customized experience and offers
4. Help resort kitchen to plan the inventory and food quantity to be prepared in advance

Given the information related to resort, club member, reservation etc. the task is to predict average spend per room night on food and beverages for the each reservation in the test set.


## Evaluation Metric
Submissions are evaluated on 100 * Root Mean Squared Error (RMSE) (https://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluationerror-metrics/) on the variable amount_spent_per_room_night_scaled


### My Approach
I have used R studio for my pre-processing. Post feature engineering, I implemented mutiple machine learning models using H2o library in R (https://www.h2o.ai/)

GBM turned to out to be best model in my case.
I got a RMSE of 96.550  in public leaderboard and stood at Rank 179.
In private leaderboard, RMSE is 97.684 and my rank is 189


## Appendix
You can refer to Club_mahindra_code.R for my code.
Refer to Club_Mahindra_Flow.pdf for plots along with code and comments
Refer to var_importance.JPG for important variables. 

## License
[MIT](https://choosealicense.com/licenses/mit/)
