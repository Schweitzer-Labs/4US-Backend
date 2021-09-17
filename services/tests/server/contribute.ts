const express = require("express");
const cors = require("cors");
import lambda from "../../src/platform-contribute.lambda";

const app = express();

app.use(cors());

app.use(express.json());

app.post("/contribute", async (req, res) => {
  const body = JSON.stringify(req.body);
  const lambdaRes = await lambda({ body });
  res.statusCode = lambdaRes.statusCode;
  res.json(JSON.parse(lambdaRes.body));
});

export const bootstrapContribute = () => app.listen(4001);
