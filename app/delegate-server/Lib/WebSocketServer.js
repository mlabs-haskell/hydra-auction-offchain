import WebSocket, { WebSocketServer } from "ws";

export const newWebSocketServer = (options) => () => {
  return new WebSocketServer(options);
};

export const onConnect = (wss) => (cb) => () => {
  wss.on("connection", (ws, req) => {
    console.log("conn url: ", req.url);
    cb(ws)();
  });
};

export const sendMessage = (ws) => (message) => () => {
  ws.send(message);
};

export const broadcastMessage = (wss) => (message) => () => {
  wss.clients.forEach((ws) => {
    if (ws.readyState === WebSocket.OPEN) {
      ws.send(message, { binary: false });
    }
  });
};

export const closeWebSocketServer = (wss) => (done) => () => {
  wss.clients.forEach((ws) => ws.terminate());
  wss.close(done);
};
