import WebSocket, { WebSocketServer } from "ws";

export const newWebSocketServer = (options) => () => {
  return new WebSocketServer(options);
};

export const onConnect = (wss) => (cb) => () => {
  wss.on("connection", function (ws) {
    cb(ws)();
  });
};

export const sendMessage = (ws) => (message) => () => {
  ws.send(message);
};

export const broadcastMessage = (wss) => (message) => () => {
  wss.clients.forEach((client) => {
    if (client.readyState === WebSocket.OPEN) {
      client.send(message, { binary: false });
    }
  });
};

export const closeWebSocketServer = (wss) => (done) => () => {
  wss.close(done);
};
